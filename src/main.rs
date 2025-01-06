use std::fs;
use std::path::PathBuf;
use std::fmt::Display;
use std::iter::Peekable;
use std::str::Chars;
use std::process::exit;
use clap::{Parser, Subcommand};
use miette::{miette, Result};

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
    Minus,
    Semicolon,
    Whitespace,
    // 2字符Token
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    // 除号和注释
    Slash,
    Comment(String),
    String(String),
    Number(String),
    Eof
}

impl Token {
    fn parse_num(&self) -> f64 {
        match self {
            Token::Number(s) => s.parse::<f64>().unwrap(),
            _ => panic!("Not a number.")
        }
    }
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
            Token::Semicolon => "SEMICOLON ; null",
            Token::Plus => "PLUS + null",
            Token::Minus => "MINUS - null",
            Token::Whitespace => "SPACE  null",
            Token::Equal => "EQUAL = null",
            Token::EqualEqual => "EQUAL_EQUAL == null",
            Token::Bang => "BANG ! null",
            Token::BangEqual => "BANG_EQUAL != null",
            Token::Less => "LESS < null",
            Token::LessEqual => "LESS_EQUAL <= null",
            Token::Greater => "GREATER > null",
            Token::GreaterEqual => "GREATER_EQUAL >= null",
            Token::Slash => "SLASH / null",
            Token::Comment(comment) => {
                // 返回Never的分支可以和其他match分支混用
                return write!(f, "COMMENT {comment}");                
            },
            Token::String(s) => {
                return write!(f, "STRING \"{s}\" {s}");
            }
            Token::Number(s) => {
                let num = self.parse_num();
                if num == num.trunc() {
                    // 是整数时额外输出".0". 这太怪了
                    return write!(f, "NUMBER {s} {}.0", num);
                } else {
                    return write!(f, "NUMBER {s} {num}");
                }
            }
            Token::Eof => "EOF  null",
        }) 
    }

}

/// 使用迭代器实现的词法分析器的内部状态  
/// 'de 表示给定&str的生命周期 
pub struct Lexer<'de> {
    /// 剩余切片
    rest: Peekable<Chars<'de>>,
    /// 整个切片，用于错误提示
    whole: &'de str,
    /// 当前位置，用于错误提示
    index: usize,
    /// 行号
    line: usize,
}

impl <'de> Lexer<'de> {
    /// 初始状态
    pub fn new(input: &'de str) -> Self {
        Self {
            rest: input.chars().peekable(), 
            whole: input,
            index: 0,
            line: 1
        }
    }
}

impl Lexer<'_> {
    // 吃掉一个字符
    fn advance(&mut self) {
        let _ = self.rest.next();
        self.index += 1;
    }

    /// 基础字符串字面量（不考虑转义）
    fn parse_string(&mut self) -> Result<Token> {
        self.index += 1;    // 先消耗开始的双引号
        let start = self.index;
        while let Some(c) = self.rest.next() {
            self.index += 1;
            match c {
                '"' => return Ok(Token::String(self.whole[start..self.index-1].to_string())),
                '\n' => self.line += 1,
                _ => {}
            }
        }
        Err(miette!("[line {}] Error: Unterminated string.", self.line))
    }

    fn parse_number(&mut self) -> Result<Token> {
        let start = self.index;
        self.index += 1;    // 已经吃了一个
        let mut frac_part = false;  // 是否在小数部分
        while let Some(c) = self.rest.peek() {  // 数字只能peek，防止多取字符
            match c {
                '0'..='9' => self.advance(),
                '.' => {
                    if frac_part {
                        break;
                    } else {
                        self.advance(); // 先消耗小数点
                        match self.rest.peek() {
                            Some('0'..='9') => frac_part = true,
                            _ => return Err(miette!("[line {}] Error: Unexpected '.' in number.", self.line))
                        }
                    }
                },
                _ => break
            }
        }
        Ok(Token::Number(self.whole[start..self.index].to_string()))
    }
}

impl Iterator for Lexer<'_> {
    // 如果迭代器直接返回Token，那么出错时，next() -> Option<Token> == None
    // 在外部获取不到Lexer的内部状态以产生错误信息
    // 所以应在迭代器内部进行异常处理，返回Token或错误
    type Item = Result<Token>;

    // 引入peek(next_if_eq)
    fn next(&mut self) -> Option<Self::Item> {
        let mut consume = true; // 如果是字符串或者数字就不在最后index+1

        let ret = match self.rest.next() {
            Some('(') => Some(Ok(Token::LeftParen)),
            Some(')') => Some(Ok(Token::RightParen)),
            Some('{') => Some(Ok(Token::LeftBrace)),
            Some('}') => Some(Ok(Token::RightBrace)),
            Some('*') => Some(Ok(Token::Star)),
            Some('.') => Some(Ok(Token::Dot)),
            Some(',') => Some(Ok(Token::Comma)),
            Some(';') => Some(Ok(Token::Semicolon)),
            Some('-') => Some(Ok(Token::Minus)),
            Some('+') => Some(Ok(Token::Plus)),
            Some('\r') | Some(' ') | Some('\t') => Some(Ok(Token::Whitespace)),
            Some('\n') => {
                self.line += 1;
                Some(Ok(Token::Whitespace))
            }
            Some('=') => {
                if Some('=') == self.rest.next_if_eq(&'=') {
                    // next_if_eq相当于peek -> next
                    self.index += 1;
                    Some(Ok(Token::EqualEqual))
                } else {
                    // 否则不会消耗下一个字符
                    Some(Ok(Token::Equal))
                }
            }
            Some('!') => {
                if Some('=') == self.rest.next_if_eq(&'=') {
                    self.index += 1;
                    Some(Ok(Token::BangEqual))
                } else {
                    Some(Ok(Token::Bang))
                }
            }
            Some('<') => {
                if Some('=') == self.rest.next_if_eq(&'=') {
                    self.index += 1;
                    Some(Ok(Token::LessEqual))
                } else {
                    Some(Ok(Token::Less))
                }
            }
            Some('>') => {
                if Some('=') == self.rest.next_if_eq(&'=') {
                    self.index += 1;
                    Some(Ok(Token::GreaterEqual))
                } else {
                    Some(Ok(Token::Greater))
                }
            }
            Some('/') => {
                if Some('/') == self.rest.next_if_eq(&'/') {
                    self.index += 1;
                    // 注释!
                    let comment: String = self.rest
                        .by_ref()
                        .take_while(|c| *c != '\r' && *c != '\n')
                        .collect();
                    self.index += comment.chars().count();
                    
                    if !cfg!(windows) {
                        // 上面这种判断，linux系统下会把换行符吃掉
                        self.line += 1;
                    }
                    Some(Ok(Token::Comment(comment)))
                } else {
                    Some(Ok(Token::Slash))
                }
            }
            Some('"') => {
                consume = false;
                Some(self.parse_string())
            },
            Some('0'..='9') => {
                consume = false;
                Some(self.parse_number())
            }
            Some(ch) => {
                Some(Err(
                    miette! { "[line {}] Error: Unexpected character: {ch}", self.line }
                ))
            }
            None => None
        };
        if consume {
            self.index += 1;
        }
        ret
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
            let mut is_err = false;

            // Uncomment this block to pass the first stage
            if !file_contents.is_empty() {
                let lexer = Lexer::new(&file_contents);
                for token in lexer {
                    match token {
                        // 不打印的暂且在这里判断
                        Ok(Token::Whitespace | Token::Comment(_)) => {}
                        Ok(tok) => {
                            println!("{tok}");
                        }
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
