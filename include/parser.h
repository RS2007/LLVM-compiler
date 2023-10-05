#pragma once
#include "lexer.h"
#include <algorithm>
#include <map>

#define assertToken(tokenIt, type) assert((*tokenIt)->tokenType == type)
#define inMapAssert(map, key) assert(map.find(key) != map.end());

enum class NodeType {
  Program,
  Statement,
  Expression,
};

enum class StatementType { Let, Block, Return, Expression };

enum class Precedence {
  Lowest,
  Equals,
  Lessgreater,
  Sum,
  Product,
  Prefix,
  Call,
  Index
};

enum class ExpressionType {
  Number,
  Identifier,
  Prefix,
  Infix,
  If,
  Function,
  Call,
  String,
  Array,
  Index,
};

class ExpressionNode;

class StringExpression {
public:
  std::string value;
  StringExpression(std::string value) { this->value = value; }
};

class IntegerExpression {
public:
  int intValue;
  IntegerExpression(int intValue) { this->intValue = intValue; }
};

class InfixExpression {
public:
  ExpressionNode *lhs;
  ExpressionNode *rhs;
  TokenType op;

  InfixExpression(ExpressionNode *lhs, TokenType op, ExpressionNode *rhs) {
    this->lhs = lhs;
    this->op = op;
    this->rhs = rhs;
  }
};

class ExpressionNode {
public:
  ExpressionType type;
  StringExpression *stringExp;
  IntegerExpression *integerExp;
  InfixExpression *infixExpr;
  ExpressionNode(StringExpression *stringExp) {
    this->type = ExpressionType::String;
    this->stringExp = stringExp;
  }
  ExpressionNode(IntegerExpression *integerExp) {
    this->type = ExpressionType::Number;
    this->integerExp = integerExp;
  }
  ExpressionNode(InfixExpression *infixExpression) {
    this->type = ExpressionType::Infix;
    this->infixExpr = infixExpression;
  }
};

class LetStatement {
public:
  std::string identifier;
  ExpressionNode *value;
  LetStatement() {}
};

class StatementNode {
public:
  StatementType type;
  LetStatement *letStatement;
  StatementNode(LetStatement *letStatement) {
    this->type = StatementType::Let;
    this->letStatement = letStatement;
  }
};

class ProgramNode {
public:
  std::vector<StatementNode *> statements;
};

class Node {
public:
  NodeType type;
  ProgramNode program;
  StatementNode statement;
  ExpressionNode expression;
};

class Parser;
typedef ExpressionNode *(*prefixParseFunction)(Precedence);
typedef ExpressionNode *(*infixParseFunction)(Precedence, ExpressionNode *);

static std::map<TokenType, Precedence> precedenceMap = {
    {TokenType::Identifier, Precedence::Lowest},
    {TokenType::Number, Precedence::Lowest},
    {TokenType::Plus, Precedence::Sum},
    {TokenType::Semicolon, Precedence::Lowest},
};

class Parser {
public:
  Parser(std::vector<Token *> tokens) { this->tokens = tokens; }

  ProgramNode *parse() {
    auto programNode = new ProgramNode();
    for (tokenIt = tokens.begin(); tokenIt < tokens.end();) {
      switch ((*tokenIt)->tokenType) {
      case TokenType::Let: {
        LetStatement *letStatement = new LetStatement();
        tokenIt++; // move to the identifier
                   //
        assertToken(tokenIt, TokenType::Identifier);
        std::string identifierString = (*tokenIt)->identifierValue;

        tokenIt++; // move to equal
        assertToken(tokenIt, TokenType::Equal);
        tokenIt++; // move to value expression

        ExpressionNode *value =
            parseExpression((*tokenIt)->tokenType, Precedence::Lowest);

        assertToken(tokenIt, TokenType::Semicolon);
        tokenIt++;

        letStatement->value = value;
        letStatement->identifier = identifierString;

        StatementNode *statement = new StatementNode(letStatement);
        programNode->statements.push_back(statement);
      }
      }
    }
    return programNode;
  }

  ExpressionNode *parseExpression(TokenType type, Precedence precedence) {
    ExpressionNode *expressionNode;
    auto lhs = parsePrefix(precedence);
    if (lhs == nullptr) {
      return nullptr;
    }
    auto precedenceMapEntry = precedenceMap.find((*tokenIt)->tokenType);
    inMapAssert(precedenceMap, (*tokenIt)->tokenType);
    auto nextPrecedence = precedenceMapEntry->second;
    while (!((((*tokenIt)->tokenType) == TokenType::Semicolon) &&
             precedence < nextPrecedence)) {
      std::vector<TokenType> hasInfix = {TokenType::Plus, TokenType::Minus};
      if (std::find(hasInfix.begin(), hasInfix.end(), (*tokenIt)->tokenType) !=
          hasInfix.end()) {
        lhs = parseInfix(lhs);
      }
      return lhs;
    }
    return expressionNode;
  }

private:
  std::vector<Token *> tokens;
  std::vector<Token *>::iterator tokenIt;
  ExpressionNode *parsePrefix(Precedence precedence) {
    switch ((*tokenIt)->tokenType) {
    case TokenType::Number:
      return parseInteger(precedence);
    case TokenType::String:
      return parseString(precedence);
    default:
      assert(false && "Should'nt hit here");
    }
  }
  ExpressionNode *parseInteger(Precedence precedence) {
    auto intExpression = new IntegerExpression((*tokenIt)->numValue);
    tokenIt++;
    auto expressionNode = new ExpressionNode(intExpression);
    return expressionNode;
  }

  ExpressionNode *parseString(Precedence precedence) {
    auto stringExpression = new StringExpression((*tokenIt)->stringValue);
    tokenIt++;
    auto expressionNode = new ExpressionNode(stringExpression);
    return expressionNode;
  }
  ExpressionNode *parseInfix(ExpressionNode *left) {
    switch ((*tokenIt)->tokenType) {
    case TokenType::Plus:
    case TokenType::Minus: {
      auto currentOp = (*tokenIt)->tokenType;
      tokenIt++;
      inMapAssert(precedenceMap, currentOp);
      auto rhs =
          parseExpression((*tokenIt)->tokenType,
                          precedenceMap.find((*tokenIt)->tokenType)->second);
      InfixExpression *infixExpression =
          new InfixExpression(left, currentOp, rhs);
      ExpressionNode *expressionNode = new ExpressionNode(infixExpression);
      return expressionNode;
    }
    default:
      std::cout << (*tokenIt)->tokenType << "\n";
      assert(false && "Should'nt hit this");
    }
  }
};
