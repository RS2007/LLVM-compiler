#pragma once

#include <assert.h>
#include <cctype>
#include <cstdlib>
#include <iostream>
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
  Comma
};

std::ostream &operator<<(std::ostream &strm, TokenType tt) {
  const std::string tokenNames[] = {
      "Number", "String",  "Identifier", "LParen",    "RParen",
      "Let",    "LSquare", "RSquare",    "Semicolon", "Plus",
      "Minus",  "Equal",   "Comma",
  };
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
  std::vector<Token *> lex(std::string input) {
    std::vector<Token *> tokens;
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
        auto token = new Token(number);
        tokens.push_back(token);
      } else if (*it == '"') {
        it++;
        std::string stringValue;
        while (*it != '"') {
          stringValue += *it;
          it++;
        }
        auto token = new Token(TokenType::String, stringValue);
        tokens.push_back(token);
        it++;
      } else if (*it == '(') {
        auto token = new Token(TokenType::LParen);
        tokens.push_back(token);
        it++;
      } else if (*it == ')') {
        auto token = new Token(TokenType::RParen);
        tokens.push_back(token);
        it++;
      } else if (*it == '[') {
        auto token = new Token(TokenType::LSquare);
        tokens.push_back(token);
        it++;
      } else if (*it == ']') {
        auto token = new Token(TokenType::RSquare);
        tokens.push_back(token);
        it++;
      } else if (*it == ';') {
        auto token = new Token(TokenType::Semicolon);
        tokens.push_back(token);
        it++;
      } else if (*it == '+') {
        auto token = new Token(TokenType::Plus);
        tokens.push_back(token);
        it++;
      } else if (*it == '-') {
        auto token = new Token(TokenType::Minus);
        tokens.push_back(token);
        it++;
      } else if (*it == '=') {
        auto token = new Token(TokenType::Equal);
        tokens.push_back(token);
        it++;
      } else if (*it == ',') {
        auto token = new Token(TokenType::Comma);
        tokens.push_back(token);
        it++;
      } else {
        std::string identifierValue;
        while (isalpha(*it)) {
          identifierValue += *it;
          it++;
        }
        Token *token;
        if (identifierValue == "let") {
          token = new Token(TokenType::Let);
        } else {
          token = new Token(TokenType::Identifier, identifierValue);
        }
        tokens.push_back(token);
      }
    }
    return tokens;
  }
};
