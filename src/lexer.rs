use std::fmt::Display;
use std::iter::Peekable;
use std::str::Chars;
use std::default::Default;
use miette::{miette, Result};
use lazy_static::lazy_static;
use serde::{Serialize, Deserialize};

lazy_static! {
    pub static ref RESERVED_WORDS: Vec<&'static str> = vec![
        "and", "class", "else", "false",
        "for", "fun", "if", "nil",
        "or", "print", "return", "super",
        "this", "true", "var", "while"
    ];
}
static WHITESPACE: &str = " \t\r\n";

/// Token的值（字面量）定义，不使用引用
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
pub enum Literal {
    #[default]
    Null,
    String(String),
    Number(f64)
}

impl Literal {
    pub fn is_null(&self) -> bool {
        self == &Literal::Null
    }

    pub fn as_str(&self) -> &str {
        match self {
            Literal::String(s) => s,
            _ => panic!("Not a String")
        }
    }

    pub fn as_num(&self) -> f64 {
        match self {
            Literal::Number(n) => *n,
            _ => panic!("Not a Number")
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Null => write!(f, "null"),
            Literal::String(s) => write!(f, "{}", s),
            Literal::Number(n) => {
                if *n == n.trunc() {
                    // 如果是整数，在Rust输出后面加上.0
                    write!(f, "{n}.0")
                } else {
                    write!(f, "{n}")
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct Token {
    /// 值
    pub value: Literal,
    /// Token类型，为了简化不使用Enum，代价是判断变慢
    pub kind: String,
    /// Token对应的单词，用于输出结果
    pub lexeme: String,
    /// 行数
    pub line: usize,
    /// lexeme开始的行内位置
    pub col: usize
}

impl Token {
    pub fn explain(&self) -> Option<String> {
        match self.kind.as_str() {
            "COMMENT" => None,  // 不显示
            _ => Some(format!("{} {} {}", self.kind, self.lexeme, self.value))
        }
    }
}

/// 使用迭代器实现词法分析器, 在这里保存迭代器内部状态 
/// 如果要取得前一个Token可能可以用Peekable<Lexer<'_>> 
#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    /// 剩余切片
    rest: Peekable<Chars<'a>>,
    /// 整个切片，用于错误提示
    whole: &'a str,
    /// 行号，从1开始
    line: usize,
    /// 行内下标，从1开始
    col: usize,
    /// 从头开始的下标
    index: usize,
    /// 当前Token开始的列
    tok_col: usize
}

impl <'a> Lexer<'a> {
    /// 初始状态
    pub fn new(input: &'a str) -> Self {
        Self {
            rest: input.chars().peekable(), 
            whole: input,
            line: 1,
            col: 1,
            index: 0,
            tok_col: 1
        }
    }
}

impl Lexer<'_> {
    // 返回得到的字符，并改变内部状态，指向下一个字符
    fn advance(&mut self) -> Option<char> {
        let next = self.rest.next();
        match next.as_ref() {
            Some(&'\n') => {
                self.line += 1;
                self.col = 1;
                self.index += 1;
            }
            Some(_) => {
                self.col += 1;
                self.index += 1;
            }
            None => {}
        }
        next
    }

    #[inline]
    fn adv(&mut self) {
        let _ = self.advance();
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.rest.peek() {
            if WHITESPACE.contains(*c) {
                self.adv();
            } else {
                break;
            }
        }
        
    }

    /// 生成简单Token（没有值），不改变内部状态
    pub fn simple_token(&self, kind: &str, lexeme: &str) -> Result<Token> {
        Ok(Token {
            value: Literal::Null,
            kind: kind.to_string(),
            lexeme: lexeme.to_string(),
            line: self.line,
            col: self.tok_col
        })
    }

    /// 基础字符串字面量（不考虑转义）  
    /// 进入函数时没有消耗任何字符
    fn parse_string(&mut self) -> Result<Token> {
        let start = self.index;
        let start_line = self.line;
        self.adv(); // eat starting "
        while let Some(c) = self.advance() {
            if c == '"' {
                let lexeme = self.whole[start..self.index].to_string();
                return Ok(Token {
                    value: Literal::String(lexeme[1..lexeme.len()-1].to_string()),
                    lexeme,
                    kind: "STRING".to_string(),
                    line: start_line,
                    col: self.tok_col
                });
            }
        }
        Err(miette!("[line {}] Error: Unterminated string.", self.line))
    }

    fn parse_number(&mut self) -> Result<Token> {
        let start = self.index;
        let start_line = self.line;
        let mut frac_part = false;  // 是否在小数部分
        while let Some(c) = self.rest.peek() {  // 数字只能peek，防止多取字符
            match c {
                '0'..='9' => self.adv(),
                '.' => {
                    if frac_part {
                        break;
                    } else {
                        self.adv(); // 先消耗小数点
                        match self.rest.peek() {
                            Some('0'..='9') => frac_part = true,
                            _ => return Err(miette!("[line {}] Error: Unexpected '.' in number.", self.line))
                        }
                    }
                },
                _ => break
            }
        }
        let lexeme = self.whole[start..self.index].to_string();
        Ok(Token {
            value: Literal::Number(lexeme.parse::<f64>().unwrap()),
            lexeme,
            kind: "NUMBER".to_string(),
            line: start_line,
            col: self.tok_col
        })
    }

    fn parse_ident(&mut self) -> Result<Token> {
        let start = self.index;
        while let Some(c) = self.rest.peek() {
            match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => self.adv(),
                _ => break
            }
        }
        let ident = self.whole[start..self.index].to_string();
        if RESERVED_WORDS.contains(&ident.as_str()) {
            self.simple_token(&ident.to_uppercase(), &ident)
        } else {
            self.simple_token("IDENTIFIER", &ident)
        }        
    }
}

impl Iterator for Lexer<'_> {
    // 如果迭代器直接返回Token，那么出错时，next() -> Option<Token> == None
    // 在外部获取不到Lexer的内部状态以产生错误信息
    // 所以应在迭代器内部进行异常处理，返回Token或错误
    type Item = Result<Token>;

    // 检查第一个字符统一用peek
    fn next(&mut self) -> Option<Self::Item> {
        let mut default_advance = true;
        self.skip_whitespace(); // 跳过空白
        self.tok_col = self.col; // 记录当前token的起始位置

        let ret = match self.rest.peek() {
            Some('(') => Some(self.simple_token("LEFT_PAREN", "(")),
            Some(')') => Some(self.simple_token("RIGHT_PAREN", ")")),
            Some('{') => Some(self.simple_token("LEFT_BRACE", "{")),
            Some('}') => Some(self.simple_token("RIGHT_BRACE", "}")),
            Some('*') => Some(self.simple_token("STAR", "*")),
            Some('.') => Some(self.simple_token("DOT", ".")),
            Some(',') => Some(self.simple_token("COMMA", ",")),
            Some(';') => Some(self.simple_token("SEMICOLON", ";")),
            Some('-') => Some(self.simple_token("MINUS", "-")),
            Some('+') => Some(self.simple_token("PLUS", "+")),
            Some('=') => {
                default_advance = false; 
                self.adv();
                // 不能用next_if_eq 因为要自己维护状态
                if Some(&'=') == self.rest.peek() {
                    self.adv(); // 再消耗一个字符
                    Some(self.simple_token("EQUAL_EQUAL", "=="))
                } else {
                    // 否则不会消耗下一个字符
                    Some(self.simple_token("EQUAL", "="))
                }
            }
            Some('!') => {
                default_advance = false;
                self.adv();
                if Some(&'=') == self.rest.peek() {
                    self.adv();
                    Some(self.simple_token("BANG_EQUAL", "!="))
                } else {
                    Some(self.simple_token("BANG", "!"))
                }
            }
            Some('<') => {
                default_advance = false;
                self.adv();
                if Some(&'=') == self.rest.peek() {
                    self.adv();
                    Some(self.simple_token("LESS_EQUAL", "<="))
                } else {
                    Some(self.simple_token("LESS", "<"))
                }
            }
            Some('>') => {
                default_advance = false;
                self.adv();
                if Some(&'=') == self.rest.peek() {
                    self.adv();
                    Some(self.simple_token("GREATER_EQUAL", ">="))
                } else {
                    Some(self.simple_token("GREATER", ">"))
                }
            }
            Some('/') => {
                default_advance = false;
                let start = self.index;
                self.adv();
                if Some(&'/') == self.rest.peek() {
                    self.adv();
                    self.skip_whitespace();
                    // 注释
                    let comment: String = self.rest
                        .by_ref()
                        .take_while(|c| *c != '\n')
                        .collect();
                    self.index += comment.chars().count() + 1;
                    let end = self.index;
                    let end_line = self.line;
                    // \n已经被消耗了，因为没有用adv,得更新状态。以后可以改
                    self.line += 1;
                    self.col = 1;
                    Some(Ok(Token {
                        value: Literal::String(comment),
                        lexeme: self.whole[start..end].to_string(),
                        kind: "COMMENT".to_string(),
                        line: end_line,
                        col: self.tok_col
                    }))
                } else {
                    Some(self.simple_token("SLASH", "/"))
                }
            }
            Some('"') => {
                default_advance = false;
                Some(self.parse_string())
            },
            Some('0'..='9') => {
                default_advance = false;
                Some(self.parse_number())
            }
            Some('A'..='Z') | Some('a'..='z') | Some('_') => {
                default_advance = false;
                Some(self.parse_ident())
            }
            Some(ch) => {
                Some(Err(
                    miette! { "[line {}] Error: Unexpected character: {ch}", self.line }
                ))
            }
            None => None
        };
        if default_advance {
            self.adv();
        }
        //println!("{ret:?}");
        
        ret
    }
}