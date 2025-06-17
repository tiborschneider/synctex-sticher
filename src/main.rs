use std::{collections::HashSet, path::PathBuf};

mod latex;
mod synctex;

/// Combine multiple synctex files
#[derive(Debug, clap::Parser)]
struct Args {
    /// The main synctex.gz file. This file will be overwritten
    main: String,
    /// The build directory (the same argument as passed to pdflatex -output-directory)
    #[clap(short, long, default_value = "")]
    output_directory: String,
    /// Additional latex commands that include a file. The \include command will  be added
    /// automatically. No leading backslash needed.
    #[clap(short, long)]
    include_commands: Vec<String>,
}

fn main() {
    let mut args: Args = clap::Parser::parse();

    // ensure that `include` is present in the list of include commands.
    let include = "include".to_string();
    if !args.include_commands.contains(&include) {
        args.include_commands.push(include);
    }
    // ensure that there are no leading backslashes.
    for cmd in args.include_commands.iter_mut() {
        *cmd = cmd.trim_start_matches("/").to_string();
    }

    // construct the build path
    let mut output_path = PathBuf::from(&args.main);
    output_path.pop();
    output_path.push(args.output_directory);

    let main_synctex_path = PathBuf::from(&args.main);
    let filename = main_synctex_path
        .file_name()
        .expect("Empty filename provided");
    let mut main_synctex_path = output_path.clone();
    main_synctex_path.push(filename);

    let main_file =
        synctex::File::read(&main_synctex_path).expect("Main synctex file does not exist!");

    let mut output_file = synctex::File::empty_from(&main_file);
    let mut includes = latex::Includes::new(args.include_commands);

    // iterate over all commands and do some work
    let mut content_iter = main_file.content.into_iter();
    let mut ignore_links = HashSet::new();

    while let Some(cmd) = content_iter.next() {
        if let Some(link) = cmd.link() {
            // Only include each link at most once
            if ignore_links.insert(link) {
                // potentially modify the cmd
                let filename = main_file
                    .inputs
                    .get(link.file - 1)
                    .expect("Invalid file link!");
                // check if this link points to an include command
                if let Some(filename) = includes.get_include(filename, link.line) {
                    let mut synctex_path = output_path.clone();
                    // remove the .tex ending
                    synctex_path.push(filename.trim_end_matches(".tex"));
                    // inject that file into the output file
                    if let Some(last_page) = output_file.try_inject(synctex_path) {
                        // advance until that page is finished
                        advance_until(&mut content_iter, |x| {
                            x == synctex::Command::PageEnd(last_page)
                        });
                        // go to the next command
                        continue;
                    }
                }
            }
        }
        // push the command
        output_file.content.push(cmd);
    }

    // write to the output file
    output_file.write_to_file(main_synctex_path);
}

fn advance_until<I, F>(i: &mut I, mut f: F)
where
    I: Iterator,
    F: FnMut(I::Item) -> bool,
{
    for next in i.by_ref() {
        if f(next) {
            return;
        }
    }
}
