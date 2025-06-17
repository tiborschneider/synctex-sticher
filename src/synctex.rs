use std::{
    collections::HashMap,
    io::{BufReader, BufWriter, Read},
    path::{Path, PathBuf},
};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::{
        complete::{line_ending, not_line_ending},
        digit1,
    },
    combinator::{all_consuming, eof, map, opt},
    multi::many1,
    sequence::{pair, preceded, separated_pair, terminated},
    Finish, Parser,
};
use nom_language::error::VerboseError;

type IResult<I, O, E = VerboseError<I>> = Result<(I, O), nom::Err<E>>;

#[derive(Clone, Debug)]
pub struct File {
    pub version: usize,
    pub inputs: Vec<String>,
    pub preamble: String,
    pub content: Vec<Command>,
    pub postamble: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Command {
    Offset(usize),
    PageStart(usize),
    PageEnd(usize),
    BoxStart {
        kind: BoxKind,
        link: Link,
        point: Point,
        size: Size,
    },
    BoxEnd(BoxKind),
    VoidBox {
        kind: BoxKind,
        link: Link,
        point: Point,
        size: Size,
    },
    CurrentRecord {
        link: Link,
        point: Point,
    },
    KernRecord {
        link: Link,
        point: Point,
        width: isize,
    },
    GlueRecord {
        link: Link,
        point: Point,
    },
    MathRecord {
        link: Link,
        point: Point,
    },
    RRecord {
        link: Link,
        point: Point,
        size: Size,
    },
}

impl Command {
    pub fn link(&self) -> Option<Link> {
        match self {
            Self::BoxStart { link, .. }
            | Self::VoidBox { link, .. }
            | Self::CurrentRecord { link, .. }
            | Self::KernRecord { link, .. }
            | Self::GlueRecord { link, .. }
            | Self::MathRecord { link, .. } => Some(*link),
            _ => None,
        }
    }

    pub fn set_link(&mut self, l: Link) {
        match self {
            Self::BoxStart { link, .. }
            | Self::VoidBox { link, .. }
            | Self::CurrentRecord { link, .. }
            | Self::KernRecord { link, .. }
            | Self::GlueRecord { link, .. }
            | Self::MathRecord { link, .. } => *link = l,
            _ => {}
        }
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub struct Link {
    pub file: usize,
    pub line: usize,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub struct Point {
    pub x: isize,
    pub y: isize,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub struct Size {
    pub width: isize,
    pub height: isize,
    pub depth: isize,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub enum BoxKind {
    VBox,
    HBox,
}

enum CommandOrInput<'a> {
    Command(Command),
    Input(&'a str),
}

macro_rules! wrt {
    ($w:ident, $c:expr, $fmt:literal) => {
        {
            *$c += $fmt.len();
            $w.write_all($fmt.as_bytes())
        }
    };
    ($w:ident, $c:expr, $fmt:literal, $($args:expr),*) => {
        {
            let s = format!($fmt, $($args),*);
            *$c += s.len();
            $w.write_all(s.as_bytes())
        }
    };
}

macro_rules! wrtln {
    ($w:ident, $c:expr, $fmt:literal) => {
        {
            *$c += $fmt.len() + 1;
            $w.write_all($fmt.as_bytes()).and_then(|_| $w.write_all(b"\n"))
        }
    };
    ($w:ident, $c:expr, $fmt:literal, $($args:expr),*) => {
        {
            let s = format!($fmt, $($args),*);
            *$c += s.len() + 1;
            $w.write_all(s.as_bytes()).and_then(|_| $w.write_all(b"\n"))
        }
    };
}

pub struct InputTransform(HashMap<usize, usize>);

impl InputTransform {
    pub fn transform(&self, mut command: Command) -> Command {
        if let Some(link) = command.link() {
            command.set_link(self.transform_link(link));
        }
        command
    }

    pub fn transform_link(&self, link: Link) -> Link {
        let Some(file) = self.0.get(&link.file).copied() else {
            panic!("Missing transformation rule!")
        };
        Link {
            file,
            line: link.line,
        }
    }
}

impl File {
    pub fn empty_from(other: &Self) -> Self {
        Self {
            version: other.version,
            inputs: other.inputs.clone(),
            preamble: other.preamble.clone(),
            content: Vec::new(),
            postamble: other.postamble.clone(),
        }
    }

    pub fn read(filename: impl AsRef<Path>) -> Option<Self> {
        let filename = filename.as_ref().to_str().unwrap();
        let filename = filename.trim_end_matches(".gz");
        let filename = filename.trim_end_matches(".synctex");
        let filename = PathBuf::from(format!("{filename}.synctex.gz"));

        if !filename.exists() {
            eprintln!("SyncTeX File {filename:?} does not exist");
            return None;
        }

        let mut reader = BufReader::new(flate2::read::GzDecoder::new(
            std::fs::File::open(&filename)
                .unwrap_or_else(|e| panic!("Cannot read file {filename:?}: {e}")),
        ));
        let mut main_file = String::new();
        while reader
            .read_to_string(&mut main_file)
            .expect("Error reading synctex file")
            > 0
        {}

        let result = File::parse(&main_file).finish();
        match result {
            Ok((_, f)) => Some(f),
            Err(e) => panic!("{e:#?}"),
        }
    }

    pub fn try_inject(&mut self, synctex_path: impl AsRef<Path>) -> Option<usize> {
        let synctex_path = synctex_path.as_ref();
        Self::read(synctex_path).map(|inject_file| self.inject(inject_file))
    }

    fn inject(&mut self, inject_file: Self) -> usize {
        let lut = self.merge_includes(&inject_file);

        // pop commands until we start a new page
        let mut page = loop {
            if let Command::PageStart(x) = self.content.last().expect("No page to inject in") {
                break *x;
            }
            self.content.pop();
        };
        let mut inject_page = 0;

        for cmd in inject_file
            .content
            .into_iter()
            .skip_while(|x| !matches!(x, Command::PageStart(1)))
        {
            self.content.push(match cmd {
                Command::PageStart(p) => {
                    inject_page += 1;
                    page += 1;
                    assert_eq!(p, inject_page);
                    Command::PageStart(page)
                }
                Command::PageEnd(_) => Command::PageEnd(page),
                cmd => lut.transform(cmd),
            });
        }

        page
    }

    pub fn merge_includes(&mut self, other: &Self) -> InputTransform {
        let mut lut = HashMap::new();
        for (i, file) in other.inputs.iter().enumerate() {
            let old_file_id = i + 1;
            let new_file_id = if let Some(idx) = self.inputs.iter().position(|x| x == file) {
                idx + 1
            } else {
                self.inputs.push(file.clone());
                self.inputs.len()
            };
            lut.insert(old_file_id, new_file_id);
        }
        InputTransform(lut)
    }

    fn parse(input: &str) -> IResult<&str, Self> {
        all_consuming(map(
            (
                Self::parse_version,
                Self::parse_inputs,
                Self::parse_preamble,
                Self::parse_content,
                Self::parse_postamble,
            ),
            |(version, inputs, preamble, main, postamble)| {
                let mut inputs: Vec<_> = inputs.into_iter().map(|x| x.to_string()).collect();
                let mut content = Vec::new();
                for x in main {
                    match x {
                        CommandOrInput::Command(c) => content.push(c),
                        CommandOrInput::Input(i) => inputs.push(i.to_string()),
                    }
                }
                Self {
                    version,
                    inputs,
                    preamble: preamble.to_string(),
                    content,
                    postamble,
                }
            },
        ))
        .parse(input)
    }

    pub fn write_to_file(&self, main_filename: impl AsRef<Path>) {
        let filename = main_filename.as_ref().to_str().unwrap();
        let filename = filename.trim_end_matches(".gz");
        let filename = filename.trim_end_matches(".synctex");
        let filename = PathBuf::from(format!("{filename}.synctex.gz"));

        let mut writer = BufWriter::new(flate2::write::GzEncoder::new(
            std::fs::OpenOptions::new()
                .create(true)
                .truncate(true)
                .write(true)
                .open(&filename)
                .unwrap_or_else(|e| panic!("Cannot write file {filename:?}: {e}")),
            flate2::Compression::fast(),
        ));

        self.write(&mut writer)
            .unwrap_or_else(|e| panic!("Error writing to file {filename:?}: {e}"));
    }

    pub fn write<W: std::io::Write>(&self, w: &mut W) -> Result<(), std::io::Error> {
        let mut c = 0;
        wrtln!(w, &mut c, "SyncTeX Version:{}", self.version)?;
        for (i, file) in self.inputs.iter().enumerate() {
            wrtln!(w, &mut c, "Input:{}:{file}", i + 1)?;
        }
        wrtln!(w, &mut c, "{}", self.preamble)?;
        wrtln!(w, &mut c, "Content:")?;

        for cmd in &self.content {
            cmd.write(w, &mut c)?;
        }

        wrtln!(w, &mut c, "{}", self.postamble)?;

        Ok(())
    }

    fn parse_version(input: &str) -> IResult<&str, usize> {
        terminated(preceded(tag("SyncTeX Version:"), number), line_ending).parse(input)
    }

    fn parse_inputs(input: &str) -> IResult<&str, Vec<&str>> {
        many1(parse_input_line).parse(input)
    }

    fn parse_preamble(input: &str) -> IResult<&str, &str> {
        map(take_until("Content:"), |s: &str| s.trim()).parse(input)
    }

    fn parse_content(input: &str) -> IResult<&str, Vec<CommandOrInput>> {
        preceded((tag("Content:"), line_ending), many1(CommandOrInput::parse)).parse(input)
    }

    fn parse_postamble(input: &str) -> IResult<&str, String> {
        map(
            (
                tag("Postamble:"),
                line_ending,
                take_until("Post scriptum:"),
                tag("Post scriptum:"),
                many1(line_ending),
                eof,
            ),
            |(a, b, c, d, _, _)| format!("{a}{b}{c}{d}\n"),
        )
        .parse(input)
    }
}

impl Link {
    fn parse(input: &str) -> IResult<&str, Self> {
        map(number_pair, |(file, line)| Self { file, line }).parse(input)
    }

    fn write<W: std::io::Write>(&self, w: &mut W, c: &mut usize) -> Result<(), std::io::Error> {
        wrt!(w, c, "{},{}", self.file, self.line)
    }
}

impl Point {
    fn parse(input: &str) -> IResult<&str, Self> {
        map(signed_pair, |(x, y)| Self { x, y }).parse(input)
    }

    fn write<W: std::io::Write>(&self, w: &mut W, c: &mut usize) -> Result<(), std::io::Error> {
        wrt!(w, c, "{},{}", self.x, self.y)
    }
}

impl Size {
    fn parse(input: &str) -> IResult<&str, Self> {
        map(signed_triple, |(width, height, depth)| Self {
            width,
            height,
            depth,
        })
        .parse(input)
    }

    fn write<W: std::io::Write>(&self, w: &mut W, c: &mut usize) -> Result<(), std::io::Error> {
        wrt!(w, c, "{},{},{}", self.width, self.height, self.depth)
    }
}

impl BoxKind {
    fn parse_open(input: &str) -> IResult<&str, Self> {
        let vbox = map(tag("["), |_| Self::VBox);
        let hbox = map(tag("("), |_| Self::HBox);
        alt((vbox, hbox)).parse(input)
    }
    fn parse_close(input: &str) -> IResult<&str, Self> {
        let vbox = map(tag("]"), |_| Self::VBox);
        let hbox = map(tag(")"), |_| Self::HBox);
        alt((vbox, hbox)).parse(input)
    }
    fn parse_void(input: &str) -> IResult<&str, Self> {
        let vbox = map(tag("v"), |_| Self::VBox);
        let hbox = map(tag("h"), |_| Self::HBox);
        alt((vbox, hbox)).parse(input)
    }

    fn write_open<W: std::io::Write>(
        &self,
        w: &mut W,
        c: &mut usize,
    ) -> Result<(), std::io::Error> {
        match self {
            Self::VBox => wrt!(w, c, "["),
            Self::HBox => wrt!(w, c, "("),
        }
    }

    fn write_close<W: std::io::Write>(
        &self,
        w: &mut W,
        c: &mut usize,
    ) -> Result<(), std::io::Error> {
        match self {
            Self::VBox => wrt!(w, c, "]"),
            Self::HBox => wrt!(w, c, ")"),
        }
    }

    fn write_void<W: std::io::Write>(
        &self,
        w: &mut W,
        c: &mut usize,
    ) -> Result<(), std::io::Error> {
        match self {
            Self::VBox => wrt!(w, c, "v"),
            Self::HBox => wrt!(w, c, "h"),
        }
    }
}

impl Command {
    fn parse(input: &str) -> IResult<&str, Self> {
        terminated(
            alt((
                Self::parse_offset,
                Self::parse_page_start,
                Self::parse_page_end,
                Self::parse_box_start,
                Self::parse_box_end,
                Self::parse_void_box,
                Self::parse_current_record,
                Self::parse_kern_record,
                Self::parse_glue_record,
                Self::parse_math_record,
                Self::parse_r_record,
            )),
            line_ending,
        )
        .parse(input)
    }

    fn write<W: std::io::Write>(&self, w: &mut W, c: &mut usize) -> Result<(), std::io::Error> {
        match self {
            Command::Offset(_) => {
                let old_val = *c;
                wrtln!(w, c, "!{}", c)?;
                *c -= old_val;
                Ok(())
            }
            Command::PageStart(p) => wrtln!(w, c, "{{{}", p),
            Command::PageEnd(p) => wrtln!(w, c, "}}{}", p),
            Command::BoxStart {
                kind,
                link,
                point,
                size,
            } => {
                kind.write_open(w, c)?;
                link.write(w, c)?;
                wrt!(w, c, ":")?;
                point.write(w, c)?;
                wrt!(w, c, ":")?;
                size.write(w, c)?;
                wrtln!(w, c, "")
            }
            Command::BoxEnd(kind) => {
                kind.write_close(w, c)?;
                wrtln!(w, c, "")
            }
            Command::VoidBox {
                kind,
                link,
                point,
                size,
            } => {
                kind.write_void(w, c)?;
                link.write(w, c)?;
                wrt!(w, c, ":")?;
                point.write(w, c)?;
                wrt!(w, c, ":")?;
                size.write(w, c)?;
                wrtln!(w, c, "")
            }
            Command::CurrentRecord { link, point } => {
                wrt!(w, c, "x")?;
                link.write(w, c)?;
                wrt!(w, c, ":")?;
                point.write(w, c)?;
                wrtln!(w, c, "")
            }
            Command::KernRecord { link, point, width } => {
                wrt!(w, c, "k")?;
                link.write(w, c)?;
                wrt!(w, c, ":")?;
                point.write(w, c)?;
                wrtln!(w, c, ":{}", width)
            }
            Command::GlueRecord { link, point } => {
                wrt!(w, c, "g")?;
                link.write(w, c)?;
                wrt!(w, c, ":")?;
                point.write(w, c)?;
                wrtln!(w, c, "")
            }
            Command::MathRecord { link, point } => {
                wrt!(w, c, "$")?;
                link.write(w, c)?;
                wrt!(w, c, ":")?;
                point.write(w, c)?;
                wrtln!(w, c, "")
            }
            Command::RRecord { link, point, size } => {
                wrt!(w, c, "r")?;
                link.write(w, c)?;
                wrt!(w, c, ":")?;
                point.write(w, c)?;
                wrt!(w, c, ":")?;
                size.write(w, c)?;
                wrtln!(w, c, "")
            }
        }
    }

    fn parse_offset(input: &str) -> IResult<&str, Self> {
        map(preceded(tag("!"), number), Self::Offset).parse(input)
    }

    fn parse_page_start(input: &str) -> IResult<&str, Self> {
        map(preceded(tag("{"), number), Self::PageStart).parse(input)
    }

    fn parse_page_end(input: &str) -> IResult<&str, Self> {
        map(preceded(tag("}"), number), Self::PageEnd).parse(input)
    }

    fn parse_void_box(input: &str) -> IResult<&str, Self> {
        map(
            pair(
                BoxKind::parse_void,
                separated_triple(Link::parse, tag(":"), Point::parse, tag(":"), Size::parse),
            ),
            |(kind, (link, point, size))| Self::VoidBox {
                kind,
                link,
                point,
                size,
            },
        )
        .parse(input)
    }

    fn parse_box_start(input: &str) -> IResult<&str, Self> {
        map(
            pair(
                BoxKind::parse_open,
                separated_triple(Link::parse, tag(":"), Point::parse, tag(":"), Size::parse),
            ),
            |(kind, (link, point, size))| Self::BoxStart {
                kind,
                link,
                point,
                size,
            },
        )
        .parse(input)
    }

    fn parse_box_end(input: &str) -> IResult<&str, Self> {
        map(BoxKind::parse_close, Self::BoxEnd).parse(input)
    }

    fn parse_current_record(input: &str) -> IResult<&str, Self> {
        map(
            preceded(
                tag("x"),
                separated_pair(Link::parse, tag(":"), Point::parse),
            ),
            |(link, point)| Self::CurrentRecord { link, point },
        )
        .parse(input)
    }

    fn parse_kern_record(input: &str) -> IResult<&str, Self> {
        map(
            preceded(
                tag("k"),
                separated_triple(Link::parse, tag(":"), Point::parse, tag(":"), signed),
            ),
            |(link, point, width)| Self::KernRecord { link, point, width },
        )
        .parse(input)
    }

    fn parse_glue_record(input: &str) -> IResult<&str, Self> {
        map(
            preceded(
                tag("g"),
                separated_pair(Link::parse, tag(":"), Point::parse),
            ),
            |(link, point)| Self::GlueRecord { link, point },
        )
        .parse(input)
    }

    fn parse_math_record(input: &str) -> IResult<&str, Self> {
        map(
            preceded(
                tag("$"),
                separated_pair(Link::parse, tag(":"), Point::parse),
            ),
            |(link, point)| Self::MathRecord { link, point },
        )
        .parse(input)
    }

    fn parse_r_record(input: &str) -> IResult<&str, Self> {
        map(
            preceded(
                tag("r"),
                separated_triple(Link::parse, tag(":"), Point::parse, tag(":"), Size::parse),
            ),
            |(link, point, size)| Self::RRecord { link, point, size },
        )
        .parse(input)
    }
}

impl<'a> CommandOrInput<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        let input_file = map(parse_input_line, CommandOrInput::Input);
        let command = map(Command::parse, CommandOrInput::Command);
        alt((input_file, command)).parse(input)
    }
}

fn parse_input_line(input: &str) -> IResult<&str, &str> {
    terminated(
        preceded(
            tag("Input:"),
            map(
                separated_pair(number, tag(":"), not_line_ending),
                |(_, p)| p,
            ),
        ),
        line_ending,
    )
    .parse(input)
}

fn number(input: &str) -> IResult<&str, usize> {
    map(digit1(), |x: &str| x.parse().unwrap()).parse(input)
}

fn number_pair(input: &str) -> IResult<&str, (usize, usize)> {
    separated_pair(number, tag(","), number).parse(input)
}

fn signed(input: &str) -> IResult<&str, isize> {
    map((opt(tag("-")), number), |(minus, x)| {
        if minus.is_some() {
            -(x as isize)
        } else {
            x as isize
        }
    })
    .parse(input)
}

fn signed_pair(input: &str) -> IResult<&str, (isize, isize)> {
    separated_pair(signed, tag(","), signed).parse(input)
}

fn signed_triple(input: &str) -> IResult<&str, (isize, isize, isize)> {
    separated_triple(signed, tag(","), signed, tag(","), signed).parse(input)
}

pub fn separated_triple<I, O1, O2, O3, E: nom::error::ParseError<I>, P1, Sep1, P2, Sep2, P3>(
    p1: P1,
    sep1: Sep1,
    p2: P2,
    sep2: Sep2,
    p3: P3,
) -> impl Parser<I, Output = (O1, O2, O3), Error = E>
where
    P1: Parser<I, Output = O1, Error = E>,
    Sep1: Parser<I, Error = E>,
    P2: Parser<I, Output = O2, Error = E>,
    Sep2: Parser<I, Error = E>,
    P3: Parser<I, Output = O3, Error = E>,
{
    map(
        separated_pair(p1, sep1, separated_pair(p2, sep2, p3)),
        |(a, (b, c))| (a, b, c),
    )
}
