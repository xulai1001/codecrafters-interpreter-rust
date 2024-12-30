use std::fs;
use std::path::PathBuf;
use std::fmt::Display;
use std::str::Chars;
use clap::{Parser, Subcommand};
use miette::{miette, Result, LabeledSpan};

/// 参考rat-718的样例(有问题)  
/// Token定义
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Star,
    Dot,
    Comma,
    Plus,
    Eof
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Token::LeftParen => "LEFT_PAREN ( null",
            Token::RightParen => "RIGHT_PAREN ) null",
            Token::LeftBrace => "LEFT_BRACE { null",
            Token::RightBrace => "RIGHT_BRACE } null",
            Token::Star => "STAR * null",
            Token::Dot => "DOT . null",
            Token::Comma => "COMMA , null",
            Token::Plus => "PLUS + null",
            Token::Eof => "EOF  null",
        })
    }
}

/// 使用迭代器实现的词法分析器的内部状态  
/// 'de 表示给定&str的生命周期 
pub struct Lexer<'de> {
    /// 剩余切片
    rest: Chars<'de>,
    /// 整个切片，用于错误提示
    whole: &'de str,
    /// 当前位置，用于错误提示
    index: usize
}

impl <'de> Lexer<'de> {
    /// 初始状态
    pub fn new(input: &'de str) -> Self {
        Self {
            rest: input.chars(), 
            whole: input,
            index: 0
        }
    }
}

impl Iterator for Lexer<'_> {
    // 如果迭代器直接返回Token，那么出错时，next() -> Option<Token> == None
    // 在外部获取不到Lexer的内部状态以产生错误信息
    // 所以应在迭代器内部进行异常处理，返回Token或错误
    type Item = Result<Token>;

    // peek?
    fn next(&mut self) -> Option<Self::Item> {
        match self.rest.next() {
            Some('(') => { self.index += 1; Some(Ok(Token::LeftParen)) },
            Some(')') => { self.index += 1; Some(Ok(Token::RightParen)) },
            Some('{') => { self.index += 1; Some(Ok(Token::LeftBrace)) },
            Some('}') => { self.index += 1; Some(Ok(Token::RightBrace)) },
            Some('*') => { self.index += 1; Some(Ok(Token::Star)) },
            Some('.') => { self.index += 1; Some(Ok(Token::Dot)) },
            Some(',') => { self.index += 1; Some(Ok(Token::Comma)) },
            Some('+') => { self.index += 1; Some(Ok(Token::Plus)) },
            Some(ch) => {
                Some(Err(
                    miette! {
                        labels = vec![ LabeledSpan::new(Some("here".to_string()), self.index, 1) ],
                        "Invalid token: {ch}"
                    }.with_source_code(self.whole.to_string())
                ))
            }
            None => None
        }
    }
}

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

            // Uncomment this block to pass the first stage
            if !file_contents.is_empty() {
                let lexer = Lexer::new(&file_contents);
                let res: Result<Vec<()>> = lexer.map(|token| {
                    println!("{}", token?);
                    Ok(())
                }).collect();
                let _ = res?;
                println!("EOF  null");
            } else {
                println!("EOF  null"); // Placeholder, remove this line when implementing the scanner
            }
        }
    }
    Ok(())
}
