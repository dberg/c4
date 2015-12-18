extern crate getopts;
use getopts::Options;
use std::env;

fn build_opts() -> Options {
    let mut opts = Options::new();

    opts.optflag("h", "help", "Print this help menu.");
    opts.optopt("f", "filename", "The filename to be processed.", "FILENAME");

    let input_text = "The input type when a filename is passed. Valid values are java or \
                      bytecode. If not set the type is inferred from the filename extension.";
    opts.optopt("i", "input-type", input_text, "TYPE");

    opts.optflag("s", "server", "Start server.");
    opts.optopt("p", "port", "Set server port. Defaults to 8080.", "PORT");
    opts.optopt("", "host", "Set server host. Defaults to localhost.", "HOST");

    opts
}

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} [options]" , program);
    print!("{}", opts.usage(&brief));
}

// TODO: return success or error
fn process_file(filename_opt: Option<String>, input_type_opt: Option<String>) {
    match input_type_opt {
        Some(input_type) => println!("{}", input_type),
        None => println!("TODO: read input type from filename extension"),
    }

    match filename_opt {
        Some(filename) => println!("TODO: process {}", filename),
        None => println!("Missing filename"),
    }
}

fn main() {
    // parse command line arguments
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();
    let opts = build_opts();

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };

    // help
    if matches.opt_present("h") {
        print_usage(&program, opts);
        return;
    }

    // filename options
    if matches.opt_present("f") {
        process_file(matches.opt_str("f"), matches.opt_str("i"));
        return;
    }

    // TODO: Server options.
}
