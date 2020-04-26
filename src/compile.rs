use crate::chunk::{Chunk, OpCode};
use crate::intern::StringPool;
use crate::object::Object;
use crate::value::Value;
use log::Level;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::FromPrimitive;

struct Local<'a> {
    name: Token<'a>,
    depth: isize,
}

pub struct Compiler<'a> {
    scanner: Scanner<'a>,
    parser: Parser<'a>,
    chunk: &'a mut Chunk,
    strings: &'a mut StringPool,
    locals: Vec<Local<'a>>,
    scope_depth: isize,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str, chunk: &'a mut Chunk, strings: &'a mut StringPool) -> Self {
        Self {
            scanner: Scanner::new(source.as_bytes()),
            parser: Parser::new(),
            chunk,
            strings,
            locals: Vec::new(),
            scope_depth: 0,
        }
    }

    pub fn compile(mut self) -> crate::Result<()> {
        self.advance();
        self.declaration();
        self.end_compile();

        if self.parser.had_error {
            Err(crate::Error::Compile)
        } else {
            Ok(())
        }
    }

    fn declaration(&mut self) {
        if self.eat(TokenKind::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.parser.panic_mode {
            self.synchronize();
        }
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name");
        if self.eat(TokenKind::Equal) {
            self.expression();
        } else {
            self.emit_op(OpCode::Nil);
        }

        self.consume(
            TokenKind::Semicolon,
            "Expect ';' after variable declaration",
        );

        self.define_variable(global);
    }

    fn parse_variable(&mut self, msg: &str) -> u8 {
        self.consume(TokenKind::Identifier, msg);

        self.declare_variable();
        if self.scope_depth > 0 {
            return 0;
        }

        self.identifier_constant(self.parser.previous.lexeme_str())
    }

    fn declare_variable(&mut self) {
        // Globals are implicitly declared
        if self.scope_depth == 0 {
            return;
        }

        let already_declared = self
            .locals
            .iter()
            .rev()
            .take_while(|l| l.depth == -1 || l.depth == self.scope_depth)
            .any(|l| self.parser.previous.lexeme == l.name.lexeme);

        if already_declared {
            self.parser
                .error("Variable with this name already declared in this scope");
        }

        let name = self.parser.previous.clone();
        self.add_local(name);
    }

    fn define_variable(&mut self, global: u8) {
        if self.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        self.emit_op(OpCode::DefineGlobal);
        self.emit_byte(global);
    }

    fn add_local(&mut self, name: Token<'a>) {
        let local = Local { name, depth: -1 };
        self.locals.push(local);
    }

    fn mark_initialized(&mut self) {
        self.locals.last_mut().unwrap().depth = self.scope_depth;
    }

    fn identifier_constant(&mut self, s: &str) -> u8 {
        let s = Object::new_string(s, self.strings);
        self.make_constant(s.into())
    }

    fn statement(&mut self) {
        if self.eat(TokenKind::Print) {
            self.print_statement();
        } else if self.eat(TokenKind::For) {
            self.for_statement();
        } else if self.eat(TokenKind::If) {
            self.if_statement();
        } else if self.eat(TokenKind::While) {
            self.while_statement();
        } else if self.eat(TokenKind::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn block(&mut self) {
        while !self.check(TokenKind::RightBrace) && !self.check(TokenKind::Eof) {
            self.declaration();
        }

        self.consume(TokenKind::RightBrace, "Expect '}' after block");
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        while let Some(local) = self.locals.pop() {
            if local.depth > self.scope_depth {
                self.emit_op(OpCode::Pop);
            } else {
                self.locals.push(local);
                break;
            }
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenKind::Semicolon, "Expect ';' after value");
        self.emit_op(OpCode::Print);
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(TokenKind::LeftParen, "Expect '(' after 'for'");

        if self.eat(TokenKind::Semicolon) {
            // No initializer
        } else if self.eat(TokenKind::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.chunk.code.len();

        let mut exit_jump = None;
        if !self.eat(TokenKind::Semicolon) {
            self.expression();
            self.consume(TokenKind::Semicolon, "Expect ';' after value");

            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse));
            self.emit_op(OpCode::Pop);
        }

        if !self.eat(TokenKind::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump);

            let increment_start = self.chunk.code.len();
            self.expression();
            self.emit_op(OpCode::Pop);
            self.consume(TokenKind::RightParen, "Expect ')' after 'for' clauses");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();

        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit_op(OpCode::Pop);
        }

        self.end_scope();
    }

    fn if_statement(&mut self) {
        self.consume(TokenKind::LeftParen, "Expect '(' after 'if'");
        self.expression();
        self.consume(TokenKind::RightParen, "Expect ')' after condition");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_op(OpCode::Pop);
        self.statement();

        let else_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(then_jump);
        self.emit_op(OpCode::Pop);

        if self.eat(TokenKind::Else) {
            self.statement();
        }

        self.patch_jump(else_jump);
    }

    fn while_statement(&mut self) {
        let loop_start = self.chunk.code.len();
        self.consume(TokenKind::LeftParen, "Expect '(' after 'while'");
        self.expression();
        self.consume(TokenKind::RightParen, "Expect ')' after 'while'");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);

        self.emit_op(OpCode::Pop);
        self.statement();

        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_op(OpCode::Pop);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenKind::Semicolon, "Expect ';' after expression");
        self.emit_op(OpCode::Pop);
    }

    fn synchronize(&mut self) {
        use TokenKind::*;
        self.parser.panic_mode = false;

        while self.parser.current.kind != Eof {
            if let Semicolon = self.parser.previous.kind {
                return;
            }

            match self.parser.current.kind {
                Class | Fun | Var | For | If | While | Print | Return => return,
                _ => {}
            }

            self.advance();
        }
    }

    fn expression(&mut self) {
        self.parse_precendence(Precedence::Assignment);
    }

    fn parse_precendence(&mut self, precedence: Precedence) {
        self.advance();
        let prefix_rule = match self.get_rule(self.parser.previous.kind).prefix {
            Some(rule) => rule,
            None => {
                self.parser.error("Expect expression");
                return;
            }
        };

        let can_assign = precedence <= Precedence::Assignment;
        prefix_rule(self, can_assign);

        while precedence <= self.get_rule(self.parser.current.kind).precedence {
            self.advance();
            let infix_rule = self.get_rule(self.parser.previous.kind).infix.unwrap();
            infix_rule(self, can_assign);
        }

        if can_assign && self.eat(TokenKind::Equal) {
            self.parser.error("Invalid assignment target");
        }
    }

    fn get_rule(&mut self, operator: TokenKind) -> ParseRule<'a> {
        macro_rules! p {
            () => {
                p!(None, None, None)
            };
            ($prefix: expr, Some $infix: ident, $precedence: ident) => {
                p!($prefix, Some(Box::new(Self::$infix)), $precedence)
            };
            (Some $prefix: ident, $infix: expr, $precedence: ident) => {
                p!(Some(Box::new(Self::$prefix)), $infix, $precedence)
            };
            (Some $prefix: ident, Some $infix: ident, $precedence: ident) => {
                p!(
                    Some(Box::new(Self::$prefix)),
                    Some(Box::new(Self::$infix)),
                    $precedence
                )
            };
            ($prefix: expr, $infix: expr, $precedence: ident) => {
                ParseRule {
                    prefix: $prefix,
                    infix: $infix,
                    precedence: Precedence::$precedence,
                }
            };
        }

        use TokenKind::*;
        match operator {
            LeftParen => p!(Some grouping, None, None),
            RightParen => p!(),
            LeftBrace => p!(),
            RightBrace => p!(),
            Comma => p!(),
            Dot => p!(),
            Minus => p!(Some unary, Some binary, Term),
            Plus => p!(None, Some binary, Term),
            Semicolon => p!(),
            Slash => p!(None, Some binary, Factor),
            Star => p!(None, Some binary, Factor),
            Bang => p!(Some unary, None, None),
            BangEqual => p!(None, Some binary, Equality),
            Equal => p!(),
            EqualEqual => p!(None, Some binary, Equality),
            Greater => p!(None, Some binary, Comparison),
            GreaterEqual => p!(None, Some binary, Comparison),
            Less => p!(None, Some binary, Comparison),
            LessEqual => p!(None, Some binary, Comparison),
            Identifier => p!(Some variable, None, None),
            String => p!(Some string, None, None),
            Number => p!(Some number, None, None),
            And => p!(None, Some and, And),
            Class => p!(),
            Else => p!(),
            False => p!(Some literal, None, None),
            For => p!(),
            Fun => p!(),
            If => p!(),
            Nil => p!(Some literal, None, None),
            Or => p!(None, Some or, Or),
            Print => p!(),
            Return => p!(),
            Super => p!(),
            This => p!(),
            True => p!(Some literal, None, None),
            Var => p!(),
            While => p!(),
            Error => p!(),
            Eof => p!(),
        }
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.parser.previous.lexeme_str(), can_assign);
    }

    fn named_variable(&mut self, name: &str, can_assign: bool) {
        let (arg, get_op, set_op) = match self.resolve_local(name) {
            Some(arg) => (arg, OpCode::GetLocal, OpCode::SetLocal),
            None => {
                let arg = self.identifier_constant(name);
                (arg, OpCode::GetGlobal, OpCode::SetGlobal)
            }
        };

        if can_assign && self.eat(TokenKind::Equal) {
            self.expression();
            self.emit_op(set_op);
        } else {
            self.emit_op(get_op);
        }
        self.emit_byte(arg as u8);
    }

    fn resolve_local(&mut self, name: &str) -> Option<u8> {
        let parser = &mut self.parser;
        self.locals.iter().enumerate().rev().find_map(|(i, l)| {
            if l.name.lexeme == name.as_bytes() {
                if l.depth == -1 {
                    parser.error("Cannot read local variable in its own initializer");
                }

                Some(i as u8)
            } else {
                None
            }
        })
    }

    fn string(&mut self, _can_assign: bool) {
        let s = self.parser.previous.lexeme_str();
        let s = Object::new_string(&s[1..s.len() - 1], self.strings);
        self.emit_constant(s.into());
    }

    fn literal(&mut self, _can_assign: bool) {
        match self.parser.previous.kind {
            TokenKind::False => self.emit_op(OpCode::False),
            TokenKind::Nil => self.emit_op(OpCode::Nil),
            TokenKind::True => self.emit_op(OpCode::True),
            _ => unreachable!(),
        }
    }

    fn number(&mut self, _can_assign: bool) {
        let value = self.parser.previous.lexeme_str().parse();
        match value {
            Ok(v) => self.emit_constant(Value::Double(v)),
            Err(_) => {
                self.parser.error("Not a valid number");
                return;
            }
        };
    }

    fn grouping(&mut self, _can_assign: bool) {
        self.expression();
        self.consume(TokenKind::RightParen, "Expect ')' after expression");
    }

    fn unary(&mut self, _can_assign: bool) {
        let operator = self.parser.previous.kind;

        // Compile the operand
        self.parse_precendence(Precedence::Unary);

        // Emit the operator instruction
        match operator {
            TokenKind::Bang => self.emit_op(OpCode::Not),
            TokenKind::Minus => self.emit_op(OpCode::Negate),
            _ => unreachable!(),
        }
    }

    fn binary(&mut self, _can_assign: bool) {
        // Remember the operator
        let operator = self.parser.previous.kind;

        // Compile the right operand
        let rule = self.get_rule(operator);
        self.parse_precendence(rule.precedence.next_higher().unwrap());

        // Emit the operator instruction
        use TokenKind::*;
        match operator {
            BangEqual => self.emit_op2(OpCode::Equal, OpCode::Not),
            EqualEqual => self.emit_op(OpCode::Equal),
            Greater => self.emit_op(OpCode::Greater),
            GreaterEqual => self.emit_op2(OpCode::Less, OpCode::Not),
            Less => self.emit_op(OpCode::Less),
            LessEqual => self.emit_op2(OpCode::Greater, OpCode::Not),
            Plus => self.emit_op(OpCode::Add),
            Minus => self.emit_op(OpCode::Subtract),
            Star => self.emit_op(OpCode::Multiply),
            Slash => self.emit_op(OpCode::Divide),
            _ => unreachable!(),
        }
    }

    fn and(&mut self, _can_assign: bool) {
        let end_jump = self.emit_jump(OpCode::Jump);

        self.emit_op(OpCode::Pop);
        self.parse_precendence(Precedence::And);

        self.patch_jump(end_jump);
    }

    fn or(&mut self, _can_assign: bool) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(else_jump);
        self.emit_op(OpCode::Pop);

        self.parse_precendence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if !self.check(kind) {
            return false;
        }
        self.advance();
        true
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.parser.current.kind == kind
    }

    fn advance(&mut self) {
        std::mem::swap(&mut self.parser.previous, &mut self.parser.current);

        loop {
            self.parser.current = self.scanner.scan_token();
            if let TokenKind::Error = self.parser.current.kind {
                let msg = self.parser.current.lexeme_str();
                self.parser.error_at_current(msg);
            } else {
                break;
            }
        }
    }

    fn consume(&mut self, kind: TokenKind, msg: &str) {
        if self.parser.current.kind == kind {
            self.advance();
            return;
        }

        self.parser.error_at_current(msg);
    }

    fn end_compile(&mut self) {
        self.emit_return();
        if log_enabled!(Level::Trace) {
            self.chunk.disassemble("code", self.strings);
        }
    }

    fn emit_jump(&mut self, op: OpCode) -> usize {
        self.emit_op(op);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        self.chunk.code.len() - 2
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.chunk.code.len() - offset - 2;

        if jump > u16::max_value() as usize {
            self.parser.error("Too much code to jump over");
        }

        let jump = jump as u16;

        self.chunk.code[offset] = (jump >> 8 & 0xff) as u8;
        self.chunk.code[offset + 1] = (jump & 0xff) as u8;
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk.push_chunk(byte, self.parser.previous.line);
    }

    fn emit_byte2(&mut self, byte_1: u8, byte_2: u8) {
        self.emit_byte(byte_1);
        self.emit_byte(byte_2);
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_op(OpCode::Loop);

        let offset = self.chunk.code.len() - loop_start + 2;
        if offset > u16::max_value() as usize {
            self.parser.error("Loop body too large");
        }

        self.emit_byte(((offset >> 8) & 0xff) as u8);
        self.emit_byte((offset & 0xff) as u8);
    }

    fn emit_op(&mut self, op: OpCode) {
        self.emit_byte(op as u8);
    }

    fn emit_op2(&mut self, op_1: OpCode, op_2: OpCode) {
        self.emit_op(op_1);
        self.emit_op(op_2);
    }

    fn emit_return(&mut self) {
        self.emit_op(OpCode::Return);
    }

    fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_byte2(OpCode::Constant as u8, constant as u8);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        let mut constant = self.chunk.push_const(value);
        if constant > u8::max_value() as usize {
            self.parser.error("Too many constants in one chunk");
            constant = 0;
        }
        constant as u8
    }
}

type ParseFn<'a> = Box<dyn Fn(&mut Compiler<'a>, bool)>;

struct ParseRule<'a> {
    prefix: Option<ParseFn<'a>>,
    infix: Option<ParseFn<'a>>,
    precedence: Precedence,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, FromPrimitive, ToPrimitive)]
#[repr(u8)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    fn next_higher(self) -> Option<Self> {
        let next = (self as u8).checked_add(1)?;
        Self::from_u8(next)
    }
}

struct Parser<'a> {
    current: Token<'a>,
    previous: Token<'a>,
    had_error: bool,
    panic_mode: bool,
}

macro_rules! error_at {
    ($self: expr, $where: ident, $msg: expr) => {
        error_at!($self, &$self.$where, $msg);
    };
    ($self: expr, $token: expr, $msg: expr) => {{
        if !$self.panic_mode {
            $self.panic_mode = true;
            print!("[line {}] Error", $token.line);

            match &$token.kind {
                TokenKind::Eof => print!(" at end"),
                TokenKind::Error => {}
                _ => print!(" at '{}'", $token.lexeme_str()),
            }

            println!(": {}", $msg);
            $self.had_error = true;
        }
    }};
}

impl<'a> Parser<'a> {
    fn new() -> Parser<'a> {
        Parser {
            current: Token {
                kind: TokenKind::Eof,
                lexeme: &[],
                line: 0,
            },
            previous: Token {
                kind: TokenKind::Eof,
                lexeme: &[],
                line: 0,
            },
            had_error: false,
            panic_mode: false,
        }
    }

    fn error_at_current(&mut self, msg: &str) {
        error_at!(self, current, msg);
    }

    fn error(&mut self, msg: &str) {
        error_at!(self, previous, msg);
    }
}

pub struct Scanner<'a> {
    source: &'a [u8],
    start: usize,
    current: usize,
    line: usize,
}

macro_rules! if_match_eq {
    ($self: expr, $yes: ident, $no: ident) => {
        if $self.is_match(b'=') {
            $self.new_token(TokenKind::$yes)
        } else {
            $self.new_token(TokenKind::$no)
        }
    };
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn scan_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        self.start = self.current;

        if let Some(c) = self.advance() {
            match c {
                // Single character tokens
                b'(' => self.new_token(TokenKind::LeftParen),
                b')' => self.new_token(TokenKind::RightParen),
                b'{' => self.new_token(TokenKind::LeftBrace),
                b'}' => self.new_token(TokenKind::RightBrace),
                b';' => self.new_token(TokenKind::Semicolon),
                b',' => self.new_token(TokenKind::Comma),
                b'.' => self.new_token(TokenKind::Dot),
                b'-' => self.new_token(TokenKind::Minus),
                b'+' => self.new_token(TokenKind::Plus),
                b'/' => self.new_token(TokenKind::Slash),
                b'*' => self.new_token(TokenKind::Star),

                // Two character tokens
                b'!' => if_match_eq!(self, BangEqual, Bang),
                b'=' => if_match_eq!(self, EqualEqual, Equal),
                b'<' => if_match_eq!(self, LessEqual, Less),
                b'>' => if_match_eq!(self, GreaterEqual, Greater),

                // Literals
                b'"' => self.string(),
                b'0'..=b'9' => self.number(),
                c if is_alpha(c) => self.identifier(),

                _ => self.new_error_token("Unexpected character."),
            }
        } else {
            self.new_token(TokenKind::Eof)
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                b' ' | b'\r' | b'\t' => {
                    self.advance();
                }
                b'\n' => {
                    self.line += 1;
                    self.advance();
                }
                b'/' => match self.peek_next() {
                    Some(next) if next == b'/' => while self.is_not_match(b'\n') {},
                    _ => break,
                },
                _ => break,
            }
        }
    }

    fn is_match(&mut self, expected: u8) -> bool {
        if let Some(c) = self.peek() {
            if c == expected {
                self.current += 1;
                return true;
            }
        }

        false
    }

    fn is_not_match(&mut self, expected: u8) -> bool {
        if let Some(c) = self.peek() {
            if c != expected {
                self.current += 1;
                return true;
            }
        }

        false
    }

    fn peek(&self) -> Option<u8> {
        self.source.get(self.current).cloned()
    }

    fn peek_next(&self) -> Option<u8> {
        self.source.get(self.current + 1).cloned()
    }

    fn advance(&mut self) -> Option<u8> {
        let c = self.source.get(self.current)?;
        self.current += 1;
        Some(*c)
    }

    fn string(&mut self) -> Token<'a> {
        while let Some(c) = self.advance() {
            match c {
                b'"' => return self.new_token(TokenKind::String),
                b'\n' => self.line += 1,
                _ => {}
            }
        }

        self.new_error_token("Unterminated string")
    }

    fn number(&mut self) -> Token<'a> {
        while let Some(b'0'..=b'9') = self.peek() {
            self.advance();
        }

        if let Some(b'.') = self.peek() {
            if let Some(b'0'..=b'9') = self.peek_next() {
                self.advance();

                while let Some(b'0'..=b'9') = self.peek() {
                    self.advance();
                }
            }
        }

        self.new_token(TokenKind::Number)
    }

    fn identifier(&mut self) -> Token<'a> {
        loop {
            match self.peek() {
                Some(c) if is_alpha(c) || is_digit(c) => {
                    self.advance();
                }
                _ => break,
            }
        }

        self.new_token(self.identifier_kind())
    }

    fn identifier_kind(&self) -> TokenKind {
        match &self.source[self.start..self.current] {
            b"and" => TokenKind::And,
            b"class" => TokenKind::Class,
            b"else" => TokenKind::Else,
            b"false" => TokenKind::False,
            b"for" => TokenKind::For,
            b"if" => TokenKind::If,
            b"nil" => TokenKind::Nil,
            b"or" => TokenKind::Or,
            b"print" => TokenKind::Print,
            b"return" => TokenKind::Return,
            b"super" => TokenKind::Super,
            b"this" => TokenKind::This,
            b"true" => TokenKind::True,
            b"var" => TokenKind::Var,
            b"while" => TokenKind::While,
            _ => TokenKind::Identifier,
        }
    }

    fn new_token(&self, kind: TokenKind) -> Token<'a> {
        Token {
            kind,
            lexeme: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    fn new_error_token(&self, msg: &'a str) -> Token<'a> {
        Token {
            kind: TokenKind::Error,
            lexeme: msg.as_bytes(),
            line: self.line,
        }
    }
}

fn is_alpha(c: u8) -> bool {
    matches!(c, b'a'..=b'z' | b'A'..=b'Z' | b'_')
}

fn is_digit(c: u8) -> bool {
    matches!(c, b'0'..=b'9')
}

#[derive(Clone)]
pub struct Token<'a> {
    kind: TokenKind,
    lexeme: &'a [u8],
    line: usize,
}

impl<'a> Token<'a> {
    fn lexeme_str(&self) -> &'a str {
        std::str::from_utf8(self.lexeme).unwrap()
    }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub enum TokenKind {
    // Single character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
}
