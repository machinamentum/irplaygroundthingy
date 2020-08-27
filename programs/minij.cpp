#include "general.h"
#include "linker_object.h"
#include "ir.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#include <vector>

char *get_file_contents(char *path) {
    FILE *file = fopen(path, "rb");
    if (!file) return nullptr;

    fseek(file, 0, SEEK_END);
    auto len = ftell(file);
    assert(len != -1);
    fseek(file, 0, SEEK_SET);

    size_t amount = static_cast<size_t>(len);
    char *buffer = (char *)malloc(amount+1);

    do {
        size_t read = fread(buffer, 1, amount, file);
        amount -= read;
    } while (amount);

    buffer[len] = 0;
    return buffer;
}

struct Token {
    enum Type {
        END = 256,
        IDENTIFIER,
        INTEGER,
        FLOAT,

        KEYWORD_START,
        KEYWORD_FUNC = KEYWORD_START,

        KEYWORD_END,
    };

    u32 type;

    union {
        char  *string_value;
        u64    integer_value;
        double float_value;
    };
};

Token token(u32 type) {
    Token t;
    t.type = type;
    return t;
}

char *keywords[] = {
    "func",

};

bool starts_identifier(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_');
}

bool continues_identifer(char c) {
    return starts_identifier(c) || (c >= '0' && c <= '9');
}

bool is_whitespace(char c) {
    return c == ' ' || c == '\r' || c == '\t' || c == '\v' || c == '\n';
}

struct Lexer {
    char *buffer;

    Token next_token() {
        while (*buffer && is_whitespace(*buffer)) buffer += 1;

        if (*buffer == 0) return token(Token::END);

        if (starts_identifier(*buffer)) {
            char *end = buffer;

            while (*end) {
                if (continues_identifer(*end)) end += 1;
                else break;
            }

            auto length = end - buffer;
            u32 type = Token::IDENTIFIER;
            for (u32 i = 0; i < (Token::KEYWORD_END - Token::KEYWORD_START); ++i) {
                if (strncmp(buffer, keywords[i], length) == 0) {
                    type = Token::KEYWORD_START + i;
                    break;
                }
            }

            Token t = token(type);

            if (type == Token::IDENTIFIER) {
                char *value = (char *)malloc(length + 1); // @Leak
                memcpy(value, buffer, length);
                value[length] = 0;
                t.string_value = value;
            }
            
            buffer = end;
            return t;
        }

        Token t = token(*buffer);
        buffer += 1;
        return t;
    }

    void tokenize_text(std::vector<Token> *output) {
        Token tok;
        do {
            tok = next_token();
            output->push_back(tok);
        } while (tok.type != Token::END);
    }
};

namespace AST {
    struct Expression {

    };

    struct Scope {
        std::vector<Expression *> statements;
    };

    struct Function {
        char *name;

        Scope *body;
    };
};

struct Parser {
    std::vector<Token> tokens;
    s32 token_cursor = 0;

    void next_token() {
        if (token_cursor < (tokens.size()-1)) token_cursor += 1;
    }

    Token peek_token() {
        return tokens[token_cursor];
    }

    bool expect_and_eat(u32 type) {
        expect(type);
        next_token();
        return true;
    }

    bool expect(u32 type) {
        Token t = peek_token();
        if (t.type != type) {
            printf("Expected token %d but got %d\n", t.type);
            return false;
        }

        return true;
    }

    AST::Scope *parse_scope() {
        if (!expect_and_eat('{')) return nullptr;

        AST::Scope *scope = new AST::Scope();

        if (!expect_and_eat('}')) return nullptr;

        return scope;
    }


    AST::Function *parse_function() {
        if (!expect_and_eat(Token::KEYWORD_FUNC)) return nullptr;
        if (!expect(Token::IDENTIFIER)) return nullptr;

        AST::Function *function = new AST::Function();
        function->name = peek_token().string_value;
        next_token();

        if (!expect_and_eat('(')) return nullptr;
        // @TODO
        if (!expect_and_eat(')')) return nullptr;

        if (peek_token().type == '{') {
            function->body = parse_scope();
        } else {
            if (!expect_and_eat(';')) return nullptr;
        }

        return function;
    }
};

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: %s <source-file>\n", argv[0]);
        return -1;
    }

    char *source_text = get_file_contents(argv[1]);
    if (!source_text) {
        printf("File does not exist: %s\n", argv[1]);
        return -1;
    }

    Lexer lex;
    lex.buffer = source_text;

    Parser parser;
    lex.tokenize_text(&parser.tokens);

    for (auto t : parser.tokens) {
        if (t.type < Token::END) printf("Token: %c\n", t.type);
        else if (t.type == Token::IDENTIFIER) printf("Identifier: %s\n", t.string_value);
        else if (t.type >= Token::KEYWORD_START && t.type <= Token::KEYWORD_END) printf("Keyword: %s\n", keywords[t.type - Token::KEYWORD_START]);
    }

    Compilation_Unit unit;
    unit.target = get_host_target();

    while (parser.peek_token().type == Token::KEYWORD_FUNC) {
        AST::Function *function = parser.parse_function();

        Function *func = new Function();
        func->name = function->name;
        func->value_type = make_func_type(make_void_type());

        if (function->body) {
            Basic_Block *block = new Basic_Block();
            func->insert(block);

            IR_Manager *irm = new IR_Manager();
            irm->set_block(block);

            irm->insert_return();
        }

        unit.functions.add(func);
    }

    emit_obj_file(&unit);

    return 0;
}

