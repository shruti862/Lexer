package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"unicode"
)

type Token int

const (
	EOF = iota
	ILLEGAL
	IDENT
	INT
	SEMI // ;
    
	// Infix ops
	ADD // +
	SUB // -
	MUL // *
	DIV // /
	REM // %
	GREATER_THAN // >
	LESS_THAN    // <
	ASSIGN       // =
	KEYWORD      // New keyword token type
	STRING       // New string token type
)

var tokens = []string{
	EOF:         "EOF",
	ILLEGAL:     "ILLEGAL",
	IDENT:       "IDENT",
	INT:         "INT",
	SEMI:        ";",
	// Infix ops
	ADD:         "+",
	SUB:         "-",
	MUL:         "*",
	DIV:         "/",
	REM:         "%",
	ASSIGN:      "=",
	GREATER_THAN: ">",
	LESS_THAN:    "<",
	KEYWORD:     "KEYWORD",
	STRING:      "STRING",
}

func (t Token) String() string {
	return tokens[t]
}

type Position struct {
	line   int
	column int
}

type Lexer struct {
	pos    Position
	reader *bufio.Reader
}

func NewLexer(reader io.Reader) *Lexer {
	return &Lexer{
		pos:    Position{line: 1, column: 0},
		reader: bufio.NewReader(reader),
	}
}

func (l *Lexer) Lex() (Position, Token, string) {
	// keep looping until we return a token
	for {
		r, _, err := l.reader.ReadRune()
		if err != nil {
			if err == io.EOF {
				return l.pos, EOF, ""
			}

			// at this point there isn't much we can do, and the compiler
			// should just return the raw error to the user
			panic(err)
		}
		l.pos.column++

		switch r {
		case '\n':
			l.resetPosition()
		case ';':
			return l.pos, SEMI, ";"
		case '+':
			return l.pos, ADD, "+"
		case '-':
			return l.pos, SUB, "-"
		case '*':
			return l.pos, MUL, "*"
		case '/':
			return l.pos, DIV, "/"
		case '>':
			return l.pos, GREATER_THAN, ">"
		case '<':
			return l.pos, LESS_THAN, "<"
		case '%':
			return l.pos, REM, "%"
		case '=':
			return l.pos, ASSIGN, "="
		default:
			if unicode.IsSpace(r) {
				continue // nothing to do here, just move on
			} else if unicode.IsDigit(r) {
				// backup and let lexInt rescan the beginning of the int
				startPos := l.pos
				l.backup()
				lit := l.lexInt()
				return startPos, INT, lit
			} else if unicode.IsLetter(r) {
				// backup and let lexIdent rescan the beginning of the ident
				startPos := l.pos
				l.backup()
				lit := l.lexIdent()
				if isKeyword(lit) {
					return startPos, KEYWORD, lit
				}
				return startPos, IDENT, lit
			} else if r == '"' {
				startPos := l.pos
				lit := l.lexString()
				return startPos, STRING, lit
			} else {
				return l.pos, ILLEGAL, string(r)
			}
		}
	}
}

// Helper function to check if a string is a Go keyword
func isKeyword(s string) bool {
	// List of Go keywords
	keywords := map[string]bool{
		"break": true,
		"default": true,
		"fn": true,
		"case": true,
		"loop": true,
		"else": true,
		"goto": true,
		"print": true,
		"switch": true,
		"const": true,
		"if": true,
		"range": true,
		"type": true,
		"continue": true,
		"for": true,
		"import": true,
		"return": true,
		"var": true,
	}

	return keywords[s]
}

// lexIdent scans the input for an identifier.
// lexIdent scans the input for an identifier.
// lexIdent scans the input for an identifier, skipping characters inside double quotes.
func (l *Lexer) lexIdent() string {
	var lit string
	insideQuotes := false

	for {
		r, _, err := l.reader.ReadRune()
		if err != nil {
			if err == io.EOF {
				// At the end of the identifier
				return lit
			}
		}

		l.pos.column++
		if !insideQuotes && unicode.IsLetter(r) {
			lit = lit + string(r)
		} else if r == '"' {
			// Toggle the insideQuotes flag to handle double quotes.
			insideQuotes = !insideQuotes
			lit = lit + string(r)
		} else {
			// Scanned something not in the identifier
			l.backup()
			return lit
		}
	}
}



func (l *Lexer) resetPosition() {
	l.pos.line++
	l.pos.column = 0
}

func (l *Lexer) backup() {
	if err := l.reader.UnreadRune(); err != nil {
		panic(err)
	}

	l.pos.column--
}

// lexInt scans the input until the end of an integer and then returns the
// literal.
func (l *Lexer) lexInt() string {
	var lit string
	for {
		r, _, err := l.reader.ReadRune()
		if err != nil {
			if err == io.EOF {
				// at the end of the int
				return lit
			}
		}

		l.pos.column++
		if unicode.IsDigit(r) {
			lit = lit + string(r)
		} else {
			// scanned something not in the integer
			l.backup()
			return lit
		}
	}
}

// lexString scans the input until it finds the closing double quote and
// returns the string literal.
// lexString scans the input for a string literal enclosed in double quotes.
// lexString scans the input for a string literal enclosed in double quotes.
// lexString scans the input for a string literal enclosed in double quotes.
func (l *Lexer) lexString() string {
	var lit string

	// Add the opening double quote to the string literal.
	lit = "\""

	// Continue reading characters until we find the closing double quote.
	for {
		r, _, err := l.reader.ReadRune()
		if err != nil {
			if err == io.EOF {
				// If we reach EOF before finding the closing double quote, it's an error.
				return "Unterminated string"
			}
		}

		l.pos.column++
		lit = lit + string(r)
		if r == '"' {
			// Found the closing double quote, so we've finished scanning the string.
			return lit
		}
	}
}




func main() {
	file, err := os.Open("input.test")
	if err != nil {
		panic(err)
	}

	lexer := NewLexer(file)
	for {
		pos, tok, lit := lexer.Lex()
		if tok == EOF {
			break
	}

		fmt.Printf("%d:%d\t%s\t%s\n", pos.line, pos.column, tok, lit)
	}
}
