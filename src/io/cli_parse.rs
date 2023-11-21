use clap::Parser;

/// KOMODO but translated to rust

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct KomodoArgs {

    /// number of arguments
    #[arg(short, long, action = clap::ArgAction::Count)]
    pub count: u8,

    /// Input File Path
    pub input_file: Option<std::path::PathBuf>,
}
