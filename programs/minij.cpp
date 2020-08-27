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
        STRING,

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
        } else if (*buffer == '"') {
            buffer += 1;
            char *end = buffer;
            while (*end && *end != '"') end += 1;

            Token t = token(Token::STRING);

            auto length = end - buffer;
            char *value = (char *)malloc(length + 1); // @Leak
            t.string_value = value;

            for (char *start = buffer; start < end; start += 1) {
                if (*start == '\\') {
                    start += 1;

                    switch (*start) {
                        case 'n':
                            *value = '\n';
                            break;

                        default:
                            *value = *start;
                            break;
                    }
                } else {
                    *value = *start;
                }

                value += 1;
            }
            *value = 0;

            buffer = end+1;
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
    enum {
        INVALID,
        FUNCTION_CALL,
        LITERAL,
    };

    struct Expression {
        u32 type;
    };

    struct Literal : Expression {
        Literal() { type = LITERAL; }

        enum {
            STRING,
            INTEGER,
            FLOAT,
        };

        u32 literal_type;

        union {
            char * string_value;
            u64    integer_value;
            double float_value;
        };

    };

    struct Function_Call : Expression {
        Function_Call() { type = FUNCTION_CALL; }

        char *call_target_name;
        std::vector<Expression *> arguments;
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

    AST::Function_Call *parse_function_call() {
        if (!expect(Token::IDENTIFIER)) return nullptr;

        Token name = peek_token();
        next_token();

        AST::Function_Call *call = new AST::Function_Call();
        call->call_target_name = name.string_value;

        if (!expect_and_eat('(')) return nullptr;

        while (peek_token().type != ')') {
            call->arguments.push_back(parse_expression());
            if (peek_token().type == ',') next_token();
        }

        if (!expect_and_eat(')')) return nullptr;

        return call;
    }

    AST::Expression *parse_expression() {
        if (peek_token().type == Token::IDENTIFIER) {
            return parse_function_call();
        }

        if (peek_token().type == Token::STRING) {
            AST::Literal *lit = new AST::Literal();

            Token t = peek_token();
            lit->literal_type = AST::Literal::STRING;
            lit->string_value = t.string_value;

            next_token();
            return lit;
        }

        return nullptr;
    }

    AST::Scope *parse_scope() {
        if (!expect_and_eat('{')) return nullptr;

        AST::Scope *scope = new AST::Scope();

        while (peek_token().type != '}') {
            AST::Expression *expr = parse_expression();
            if (expr) scope->statements.push_back(expr);

            if (peek_token().type == ';') next_token();
        }

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

Value *emit_expression(AST::Expression *expr, Compilation_Unit *unit, IR_Manager *irm) {
    switch (expr->type) {
        case AST::LITERAL: {
            auto lit = static_cast<AST::Literal *>(expr);

            switch (lit->literal_type) {
                case AST::Literal::STRING : return make_string_constant(lit->string_value);
                case AST::Literal::INTEGER: return make_integer_constant(lit->integer_value);
            }
            assert(false);
            return nullptr;
        }

        case AST::FUNCTION_CALL: {
            auto call = static_cast<AST::Function_Call *>(expr);

            Array<Value *> args;

            for (auto a : call->arguments) {
                args.add(emit_expression(a, unit, irm));
            }

            Function *target = nullptr;
            for (auto func : unit->functions) {
                if (strcmp(call->call_target_name, func->name.data) == 0) {
                    target = func;
                    break;
                }
            }

            assert(target);

            return irm->insert_call(target, args);
        }
    }
}

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

    /*
    for (auto t : parser.tokens) {
        if (t.type < Token::END) printf("Token: %c\n", t.type);
        else if (t.type == Token::IDENTIFIER) printf("Identifier: %s\n", t.string_value);
        else if (t.type >= Token::KEYWORD_START && t.type <= Token::KEYWORD_END) printf("Keyword: %s\n", keywords[t.type - Token::KEYWORD_START]);
        else if (t.type == Token::STRING) printf("String: \"%s\"\n", t.string_value);
    }
    */

    std::vector<AST::Function *> functions;
    while (parser.peek_token().type == Token::KEYWORD_FUNC) {
        AST::Function *function = parser.parse_function();
        functions.push_back(function);
    }

    Compilation_Unit unit;
    unit.target = get_host_target();

    for (auto function : functions) {
        Function *func = new Function();
        func->name = function->name;
        func->value_type = make_func_type(make_void_type());
        unit.functions.add(func);

        if (function->body) {
            Basic_Block *block = new Basic_Block();
            func->insert(block);

            IR_Manager *irm = new IR_Manager();
            irm->set_block(block);

            for (auto expr: function->body->statements) {
                emit_expression(expr, &unit, irm);
            }

            irm->insert_return();
        }
    }

    // emit_obj_file(&unit);
    do_jit_and_run_program_main(&unit);

    return 0;
}

