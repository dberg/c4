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

fn main() {
    println!("Hello, world!");

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

    // TODO: Filename options.

    // TODO: Server options.
}
