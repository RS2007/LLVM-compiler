#pragma once

#include <assert.h>
#include <cctype>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <string>
#include <vector>
enum class TokenType {
    Number,
    String,
    Identifier,
    LParen,
    RParen,
    Let,
    LSquare,
    RSquare,
    Semicolon,
    Plus,
    Minus,
    Equal,
    Comma,
    Return,
    Function,
    LBrace,
    RBrace,
    IntType,
    StringType,
    VoidType,
    Colon,
    If,
    Else,
    LessThan,
    GreaterThan,
    For,
    While
};

std::ostream& operator<<(std::ostream& strm, TokenType tt) {
    const std::string tokenNames[] = {
        "Number", "String",  "Identifier", "LParen",     "RParen",
        "Let",    "LSquare", "RSquare",    "Semicolon",  "Plus",
        "Minus",  "Equal",   "Comma",      "Return",     "Function",
        "LBrace", "RBrace",  "IntType",    "StringType", "Colon",
        "If",     "Else",    "LessThan",   "GreaterThan"};
    return strm << tokenNames[static_cast<int>(tt)];
}

class Token {
   public:
    TokenType tokenType;
    int numValue;
    std::string stringValue;
    std::string identifierValue;

    Token(int numValue) {
        this->tokenType = TokenType::Number;
        this->numValue = numValue;
    }
    Token(TokenType tokenType, std::string value) {
        this->tokenType = tokenType;
        switch (tokenType) {
            case TokenType::Identifier: {
                this->identifierValue = value;
                break;
            }
            case TokenType::String: {
                this->stringValue = value;
                break;
            }
            default:
                assert(false && "Should'nt hit here");
        }
    }
    Token(TokenType tokenType) { this->tokenType = tokenType; }
};

class Lexer {
   public:
    std::vector<std::shared_ptr<Token>> lex(std::string input) {
        std::vector<std::shared_ptr<Token>> tokens;
        std::string::iterator it = input.begin();
        while (it < input.end()) {
            if (std::isspace(*it)) {
                it++;
            } else if (std::isdigit(*it)) {
                int number = 0;
                while (it != input.end() && std::isdigit(*it)) {
                    number = 10 * number + static_cast<int>(*it - '0');
                    it++;
                }
                auto token = std::make_shared<Token>(Token(number));
                tokens.emplace_back(token);
            } else if (*it == '"') {
                it++;
                std::string stringValue;
                while (*it != '"') {
                    if (*it == '\\') {
                        it++;
                        if (*it == 'n') {
                            stringValue += "\n";
                            it++;
                            continue;
                        } else {
                            stringValue += "\\";
                        }
                    }
                    stringValue += *it;
                    it++;
                }
                auto token = std::make_shared<Token>(
                    Token(TokenType::String, stringValue));
                tokens.emplace_back(token);
                it++;
            } else if (*it == '(') {
                auto token = std::make_shared<Token>(Token(TokenType::LParen));
                tokens.emplace_back(token);
                it++;
            } else if (*it == ')') {
                auto token = std::make_shared<Token>(Token(TokenType::RParen));
                tokens.emplace_back(token);
                it++;
            } else if (*it == '[') {
                auto token = std::make_shared<Token>(Token(TokenType::LSquare));
                tokens.emplace_back(token);
                it++;
            } else if (*it == ']') {
                auto token = std::make_shared<Token>(Token(TokenType::RSquare));
                tokens.emplace_back(token);
                it++;
            } else if (*it == ';') {
                auto token =
                    std::make_shared<Token>(Token(TokenType::Semicolon));
                tokens.emplace_back(token);
                it++;
            } else if (*it == '+') {
                auto token = std::make_shared<Token>(Token(TokenType::Plus));
                tokens.emplace_back(token);
                it++;
            } else if (*it == '-') {
                auto token = std::make_shared<Token>(Token(TokenType::Minus));
                tokens.emplace_back(token);
                it++;
            } else if (*it == '=') {
                auto token = std::make_shared<Token>(Token(TokenType::Equal));
                tokens.emplace_back(token);
                it++;
            } else if (*it == ',') {
                auto token = std::make_shared<Token>(Token(TokenType::Comma));
                tokens.emplace_back(token);
                it++;
            } else if (*it == '{') {
                auto token = std::make_shared<Token>(Token(TokenType::LBrace));
                tokens.emplace_back(token);
                it++;
            } else if (*it == '}') {
                auto token = std::make_shared<Token>(Token(TokenType::RBrace));
                tokens.emplace_back(token);
                it++;
            } else if (*it == ':') {
                auto token = std::make_shared<Token>(TokenType::Colon);
                tokens.emplace_back(token);
                it++;
            } else if (*it == '<') {
                auto token = std::make_shared<Token>(TokenType::LessThan);
                tokens.emplace_back(token);
                it++;
            } else if (*it == '>') {
                auto token = std::make_shared<Token>(TokenType::GreaterThan);
                tokens.emplace_back(token);
                it++;
            } else {
                std::string identifierValue;
                while (isalpha(*it)) {
                    identifierValue += *it;
                    it++;
                }
                std::shared_ptr<Token> token;
                if (identifierValue == "let") {
                    token = std::make_shared<Token>(TokenType::Let);
                } else if (identifierValue == "return") {
                    token = std::make_shared<Token>(TokenType::Return);
                } else if (identifierValue == "defun") {
                    token = std::make_shared<Token>(TokenType::Function);
                } else if (identifierValue == "int") {
                    token = std::make_shared<Token>(TokenType::IntType);
                } else if (identifierValue == "string") {
                    token = std::make_shared<Token>(TokenType::StringType);
                } else if (identifierValue == "void") {
                    token = std::make_shared<Token>(TokenType::VoidType);
                } else if (identifierValue == "if") {
                    token = std::make_shared<Token>(TokenType::If);
                } else if (identifierValue == "else") {
                    token = std::make_shared<Token>(TokenType::Else);
                } else if (identifierValue == "for") {
                    token = std::make_shared<Token>(TokenType::For);
                } else if (identifierValue == "while") {
                    token = std::make_shared<Token>(TokenType::While);
                } else {
                    token = std::make_shared<Token>(
                        Token(TokenType::Identifier, identifierValue));
                }
                tokens.emplace_back(token);
            }
        }
        return tokens;
    }
};
