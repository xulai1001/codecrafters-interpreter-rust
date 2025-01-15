use std::fs;
use std::path::PathBuf;
use std::process::exit;
use clap::{Parser, Subcommand};
use miette::Result;

mod lexer;
use lexer::*;

#[derive(Parser, Debug)]
#[command(version=None, about=None, long_about=None)]
struct Args {
    #[command(subcommand)]
    cmd: Commands
}

#[derive(Subcommand, Debug)]
enum Commands {
    Tokenize { filename: PathBuf }
}

fn main() -> Result<()> {
    let args = Args::parse();

    match args.cmd {
        Commands::Tokenize { filename } => {
            let file_contents = fs::read_to_string(&filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {:?}", filename);
                String::new()
            });
            let mut is_err = false;

            // Uncomment this block to pass the first stage
            if !file_contents.is_empty() {
                let lexer = Lexer::new(&file_contents);
                for token in lexer {
                    match token.map(|t| t.explain()) {
                        Ok(Some(tok)) => {
                            println!("{tok}");
                        }
                        Ok(None) => {}
                        Err(e) => {
                            eprintln!("{e}");
                            is_err = true;
                        }
                    }
                }
                println!("EOF  null");
                if is_err {
                    exit(65);
                }
            } else {
                println!("EOF  null"); // Placeholder, remove this line when implementing the scanner
            }
        }
    }

    Ok(())
}
