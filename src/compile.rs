use crate::chunk::OpCode;
use crate::intern::StringPool;
use crate::object::Function;
use crate::object::Object;
use crate::value::Value;
use log::Level;
use std::rc::Rc;

struct Local<'a> {
    name: Token<'a>,
    depth: isize,
}

pub struct Compiler<'a> {
    scanner: Scanner<'a>,
    parser: Parser<'a>,
    strings: &'a mut StringPool,
    locals: Vec<Local<'a>>,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str, strings: &'a mut StringPool) -> Self {
        Self {
            scanner: Scanner::new(source.as_bytes()),
            parser: Parser::new(),
            strings,
            locals: Vec::new(),
        }
    }

    pub fn compile(mut self, kind: FunctionKind) -> crate::Result<Function> {
        let mut session = CompileSession::new(
            kind,
            &mut self.scanner,
            &mut self.parser,
            self.strings,
            &mut self.locals,
            0,
        );
        session.advance();
        session.declaration();
        session.end_compile();

        if session.parser.had_error {
            Err(crate::Error::Compile)
        } else {
            Ok(session.function)
        }
    }
}

struct CompileSession<'a, 'b> {
    scanner: &'b mut Scanner<'a>,
    parser: &'b mut Parser<'a>,
    function: Function,
    kind: FunctionKind,
    strings: &'b mut StringPool,
    locals: &'b mut Vec<Local<'a>>,
    stack_top: usize,
    scope_depth: isize,
}

#[derive(PartialEq)]
pub enum FunctionKind {
    Function,
    Script,
}

macro_rules! chunk {
    ($self: expr) => {
        $self.function.chunk
    };
}

impl<'a, 'b> CompileSession<'a, 'b> {
    fn new(
        kind: FunctionKind,
        scanner: &'b mut Scanner<'a>,
        parser: &'b mut Parser<'a>,
        strings: &'b mut StringPool,
        locals: &'b mut Vec<Local<'a>>,
        stack_top: usize,
    ) -> Self {
        let mut function = Function::new();
        if kind != FunctionKind::Script {
            function.name = parser.previous.lexeme_str().to_string();
        }

        locals.push(Local {
            name: Token::new(),
            depth: 0,
        });

        Self {
            scanner,
            parser,
            function,
            kind,
            strings,
            locals,
            scope_depth: 0,
            stack_top,
        }
    }

    fn new_inner(&mut self, kind: FunctionKind) -> CompileSession<'a, '_> {
        CompileSession::new(
            kind,
            self.scanner,
            self.parser,
            self.strings,
            self.locals,
            self.locals.len(),
        )
    }

    fn declaration(&mut self) {
        if self.eat(TokenKind::Fun) {
            self.fun_declaration();
        } else if self.eat(TokenKind::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.parser.panic_mode {
            self.synchronize();
        }
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function name");
        self.mark_initialized();
        self.function(FunctionKind::Function);
        self.define_variable(global);
    }

    fn function(&mut self, kind: FunctionKind) {
        let mut inner = self.new_inner(kind);

        inner.begin_scope();
        inner.consume(TokenKind::LeftParen, "Expect '(' after function name");

        if !inner.check(TokenKind::RightParen) {
            loop {
                inner.function.arity += 1;
                if inner.function.arity > 255 {
                    inner
                        .parser
                        .error_at_current("Cannot have more than 255 parameters");
                }

                let param_constant = inner.parse_variable("Expect parameter name");
                inner.define_variable(param_constant);

                if !inner.eat(TokenKind::Comma) {
                    break;
                }
            }
        }

        inner.consume(TokenKind::RightParen, "Expect ')' after parameters");

        inner.consume(TokenKind::LeftBrace, "Expect '{' before function body");
        inner.block();

        inner.end_compile();
        let function = Object::Function(Rc::new(inner.function));
        self.emit_op(OpCode::Closure);
        let c = self.make_constant(function.into());
        self.emit_byte(c)
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
        if self.scope_depth == 0 {
            return;
        }
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
        } else if self.eat(TokenKind::Return) {
            self.return_statement();
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

        let mut loop_start = chunk!(self).code.len();

        let mut exit_jump = None;
        if !self.eat(TokenKind::Semicolon) {
            self.expression();
            self.consume(TokenKind::Semicolon, "Expect ';' after value");

            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse));
            self.emit_op(OpCode::Pop);
        }

        if !self.eat(TokenKind::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump);

            let increment_start = chunk!(self).code.len();
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

    fn return_statement(&mut self) {
        if self.kind == FunctionKind::Script {
            self.parser.error("Cannot return from top level code");
        }

        if self.eat(TokenKind::Semicolon) {
            self.emit_return();
        } else {
            self.expression();
            self.consume(TokenKind::Semicolon, "Expect ';' after return value");
            self.emit_op(OpCode::Return);
        }
    }

    fn while_statement(&mut self) {
        let loop_start = chunk!(self).code.len();
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
        self.parse_precendence(Cmp::Lte(Precedence::Assignment));
    }

    fn parse_precendence(&mut self, precedence: Cmp<Precedence>) {
        self.advance();
        let prefix_rule = match self.get_rule(self.parser.previous.kind).prefix {
            Some(rule) => rule,
            None => {
                self.parser.error("Expect expression");
                return;
            }
        };

        let can_assign = precedence.cmp(&Precedence::Assignment);
        prefix_rule(self, can_assign);

        while precedence.cmp(&self.get_rule(self.parser.current.kind).precedence) {
            self.advance();
            let infix_rule = self.get_rule(self.parser.previous.kind).infix.unwrap();
            infix_rule(self, can_assign);
        }

        if can_assign && self.eat(TokenKind::Equal) {
            self.parser.error("Invalid assignment target");
        }
    }

    fn get_rule(&mut self, operator: TokenKind) -> ParseRule<'a, 'b> {
        macro_rules! p {
            () => {
                p!(None, None, None)
            };
            ($prefix: expr, Some $infix: ident, $precedence: ident) => {
                p!($prefix, Some(Self::$infix), $precedence)
            };
            (Some $prefix: ident, $infix: expr, $precedence: ident) => {
                p!(Some(Self::$prefix), $infix, $precedence)
            };
            (Some $prefix: ident, Some $infix: ident, $precedence: ident) => {
                p!(Some(Self::$prefix), Some(Self::$infix), $precedence)
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
            LeftParen => p!(Some grouping, Some call, Call),
            RightParen => p!(),
            LeftBrace => p!(),
            RightBrace => p!(),
            Comma => p!(),
            Dot => p!(),
            Minus => p!(Some unary, Some binary, Term),
            Plus => p!(None, Some binary, Term),
            Semicolon => p!(),
            Slash | Star => p!(None, Some binary, Factor),
            Bang => p!(Some unary, None, None),
            BangEqual | EqualEqual => p!(None, Some binary, Equality),
            Equal => p!(),
            Less | LessEqual | Greater | GreaterEqual => p!(None, Some binary, Comparison),
            Identifier => p!(Some variable, None, None),
            String => p!(Some string, None, None),
            Number => p!(Some number, None, None),
            And => p!(None, Some and, And),
            Or => p!(None, Some or, Or),
            Class => p!(),
            True | False | Nil => p!(Some literal, None, None),
            For => p!(),
            Fun => p!(),
            If => p!(),
            Else => p!(),
            Print => p!(),
            Return => p!(),
            Super => p!(),
            This => p!(),
            Var => p!(),
            While => p!(),
            Error => p!(),
            Eof => p!(),
        }
    }

    fn call(&mut self, _can_assign: bool) {
        let arg_count = self.arg_list();
        self.emit_op(OpCode::Call);
        self.emit_byte(arg_count);
    }

    fn arg_list(&mut self) -> u8 {
        let mut arg_count = 0;
        if !self.check(TokenKind::RightParen) {
            loop {
                self.expression();
                if arg_count == 255 {
                    self.parser.error("Cannot have more than 255 arguments");
                }
                arg_count += 1;

                if !self.eat(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenKind::RightParen, "Expect ')' after arguments");
        arg_count
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
        self.locals
            .iter()
            .skip(self.stack_top)
            .enumerate()
            .rev()
            .find_map(|(i, l)| {
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
        self.parse_precendence(Cmp::Lte(Precedence::Unary));

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
        self.parse_precendence(Cmp::Lt(rule.precedence));

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
        self.parse_precendence(Cmp::Lte(Precedence::And));

        self.patch_jump(end_jump);
    }

    fn or(&mut self, _can_assign: bool) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(else_jump);
        self.emit_op(OpCode::Pop);

        self.parse_precendence(Cmp::Lte(Precedence::Or));
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
            chunk!(self).disassemble("code", self.strings);
        }
        self.locals.truncate(self.stack_top);
    }

    fn emit_jump(&mut self, op: OpCode) -> usize {
        self.emit_op(op);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        chunk!(self).code.len() - 2
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = chunk!(self).code.len() - offset - 2;

        if jump > u16::max_value() as usize {
            self.parser.error("Too much code to jump over");
        }

        let jump = jump as u16;

        chunk!(self).code[offset] = (jump >> 8 & 0xff) as u8;
        chunk!(self).code[offset + 1] = (jump & 0xff) as u8;
    }

    fn emit_byte(&mut self, byte: u8) {
        chunk!(self).push_chunk(byte, self.parser.previous.line);
    }

    fn emit_byte2(&mut self, byte_1: u8, byte_2: u8) {
        self.emit_byte(byte_1);
        self.emit_byte(byte_2);
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_op(OpCode::Loop);

        let offset = chunk!(self).code.len() - loop_start + 2;
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
        self.emit_op(OpCode::Nil);
        self.emit_op(OpCode::Return);
    }

    fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_byte2(OpCode::Constant as u8, constant as u8);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        let mut constant = chunk!(self).push_const(value);
        if constant > u8::max_value() as usize {
            self.parser.error("Too many constants in one chunk");
            constant = 0;
        }
        constant as u8
    }
}

type ParseFn<'a, 'b> = fn(&mut CompileSession<'a, 'b>, bool);

struct ParseRule<'a, 'b> {
    prefix: Option<ParseFn<'a, 'b>>,
    infix: Option<ParseFn<'a, 'b>>,
    precedence: Precedence,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
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
}

enum Cmp<T> {
    Lt(T),
    Lte(T),
}

impl<T: PartialOrd<T>> Cmp<T> {
    fn cmp(&self, other: &T) -> bool {
        match self {
            Self::Lt(t) => t < other,
            Self::Lte(t) => t <= other,
        }
    }
}

struct Parser<'a> {
    current: Token<'a>,
    previous: Token<'a>,
    had_error: bool,
    panic_mode: bool,
}

impl<'a> Parser<'a> {
    fn new() -> Parser<'a> {
        Parser {
            current: Token::new(),
            previous: Token::new(),
            had_error: false,
            panic_mode: false,
        }
    }

    fn error_at_current(&mut self, msg: &str) {
        error(
            &mut self.panic_mode,
            &mut self.had_error,
            &self.current,
            msg,
        );
    }

    fn error(&mut self, msg: &str) {
        error(
            &mut self.panic_mode,
            &mut self.had_error,
            &self.previous,
            msg,
        );
    }
}

fn error(panic_mode: &mut bool, had_error: &mut bool, token: &Token, msg: &str) {
    if *panic_mode {
        return;
    }

    *panic_mode = true;
    *had_error = true;

    print!("[line {}] Error", token.line);
    match &token.kind {
        TokenKind::Eof => print!(" at end"),
        TokenKind::Error => {}
        _ => print!(" at '{}'", token.lexeme_str()),
    }
    println!(": {}", msg);
}

struct Scanner<'a> {
    source: &'a [u8],
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    fn new(source: &'a [u8]) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn scan_token(&mut self) -> Token<'a> {
        use TokenKind::*;

        self.skip_whitespace();
        self.start = self.current;

        if let Some(c) = self.advance() {
            match c {
                // Single character tokens
                b'(' => self.new_token(LeftParen),
                b')' => self.new_token(RightParen),
                b'{' => self.new_token(LeftBrace),
                b'}' => self.new_token(RightBrace),
                b';' => self.new_token(Semicolon),
                b',' => self.new_token(Comma),
                b'.' => self.new_token(Dot),
                b'-' => self.new_token(Minus),
                b'+' => self.new_token(Plus),
                b'/' => self.new_token(Slash),
                b'*' => self.new_token(Star),

                // Two character tokens
                b'!' => self.if_match_eq(BangEqual, Bang),
                b'=' => self.if_match_eq(EqualEqual, Equal),
                b'<' => self.if_match_eq(LessEqual, Less),
                b'>' => self.if_match_eq(GreaterEqual, Greater),

                // Literals
                b'"' => self.string(),
                b'0'..=b'9' => self.number(),
                c if is_alpha(c) => self.identifier(),

                _ => self.new_error_token("Unexpected character."),
            }
        } else {
            self.new_token(Eof)
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

    fn if_match_eq(&mut self, yes: TokenKind, no: TokenKind) -> Token<'a> {
        if self.is_match(b'=') {
            self.new_token(yes)
        } else {
            self.new_token(no)
        }
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
            b"fun" => TokenKind::Fun,
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
    pub fn new() -> Self {
        Self {
            kind: TokenKind::Eof,
            lexeme: &[],
            line: 0,
        }
    }
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
