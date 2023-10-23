#pragma once
#include <algorithm>
#include <map>
#include <memory>
#include "lexer.h"

#define assertToken(tokenIt, type) assert((*tokenIt).get()->tokenType == type)
#define inMapAssert(map, key) assert(map.find(key) != map.end());

// Forward declarations in no particular order
class BlockStatement;
class ExpressionNode;
class StatementNode;

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
    std::shared_ptr<ExpressionNode> lhs;
    std::shared_ptr<ExpressionNode> rhs;
    TokenType op;

    InfixExpression(std::shared_ptr<ExpressionNode> lhs,
                    TokenType op,
                    std::shared_ptr<ExpressionNode> rhs) {
        this->lhs = lhs;
        this->op = op;
        this->rhs = rhs;
    }
};

class FunctionExpression {
   public:
    std::string name;
    std::shared_ptr<std::vector<std::shared_ptr<ExpressionNode>>> arguments;
    std::shared_ptr<BlockStatement> body;
    FunctionExpression() {}
    FunctionExpression(
        std::string name,
        std::shared_ptr<std::vector<std::shared_ptr<ExpressionNode>>> arguments,
        std::shared_ptr<BlockStatement> body)
        : name(name), arguments(arguments), body(body) {}
};

class IdentifierExpression {
   public:
    std::string name;
    IdentifierExpression() {}
    IdentifierExpression(std::string name) : name(name) {}
};

class ExpressionNode {
   public:
    ExpressionType type;
    std::shared_ptr<StringExpression> stringExp;
    std::shared_ptr<IntegerExpression> integerExp;
    std::shared_ptr<InfixExpression> infixExpr;
    std::shared_ptr<FunctionExpression> funcExpression;
    std::shared_ptr<IdentifierExpression> identifierExpression;
    ExpressionNode(std::shared_ptr<StringExpression> stringExp) {
        this->type = ExpressionType::String;
        this->stringExp = stringExp;
    }
    ExpressionNode(std::shared_ptr<IntegerExpression> integerExp) {
        this->type = ExpressionType::Number;
        this->integerExp = integerExp;
    }
    ExpressionNode(std::shared_ptr<InfixExpression> infixExpression) {
        this->type = ExpressionType::Infix;
        this->infixExpr = infixExpression;
    }
    ExpressionNode(std::shared_ptr<FunctionExpression> funcExpression)
        : funcExpression(funcExpression), type(ExpressionType::Function) {}
    ExpressionNode(std::shared_ptr<IdentifierExpression> identifierExpression)
        : identifierExpression(identifierExpression),
          type(ExpressionType::Identifier) {}
};

class LetStatement {
   public:
    std::string identifier;
    std::shared_ptr<ExpressionNode> value;
    LetStatement() {}
};

class BlockStatement {
   public:
    std::vector<std::shared_ptr<StatementNode>> statements;
    BlockStatement(std::vector<std::shared_ptr<StatementNode>> statements)
        : statements(statements) {}
};

class ExpressionStatement {
   public:
    std::shared_ptr<ExpressionNode> expression;
    ExpressionStatement(std::shared_ptr<ExpressionNode> expression)
        : expression(expression) {}
};

class StatementNode {
   public:
    StatementType type;
    std::shared_ptr<LetStatement> letStatement;
    std::shared_ptr<ExpressionStatement> expressionStatement;
    StatementNode() {}
    StatementNode(std::shared_ptr<LetStatement> letStatement)
        : type(StatementType::Let), letStatement(letStatement) {}
    StatementNode(std::shared_ptr<ExpressionStatement> expressionStatement) {
        this->type = StatementType::Expression;
        this->expressionStatement = expressionStatement;
    }
};

class ProgramNode {
   public:
    std::vector<std::shared_ptr<StatementNode>> statements;
};

class Node {
   public:
    NodeType type;
    std::shared_ptr<ProgramNode> program;
    std::shared_ptr<StatementNode> statement;
    std::shared_ptr<ExpressionNode> expression;
};

class Parser;
typedef ExpressionNode* (*prefixParseFunction)(Precedence);
typedef ExpressionNode* (*infixParseFunction)(Precedence, ExpressionNode*);

static std::map<TokenType, Precedence> precedenceMap = {
    {TokenType::Identifier, Precedence::Lowest},
    {TokenType::Number, Precedence::Lowest},
    {TokenType::Plus, Precedence::Sum},
    {TokenType::Minus, Precedence::Sum},
    {TokenType::Function, Precedence::Lowest},
};

class Parser {
   public:
    Parser(std::vector<std::shared_ptr<Token>> tokens) {
        this->tokens = tokens;
    }

    std::shared_ptr<ProgramNode> parse() {
        auto programNode = std::make_shared<ProgramNode>();
        std::cout << "Parsing program\n";
        for (tokenIt = tokens.begin(); tokenIt < tokens.end();) {
            std::cout << "Parsing statement\n";
            auto statement = parseStatement();
            programNode->statements.emplace_back(statement);
        }
        return programNode;
    }

    std::shared_ptr<ExpressionNode> parseExpression(Precedence precedence) {
        std::shared_ptr<ExpressionNode> expressionNode;
        auto lhs = parsePrefix(precedence);
        if (lhs == nullptr) {
            return nullptr;
        }
        if ((*tokenIt).get() == 0x0 ||
            precedenceMap.find((*tokenIt).get()->tokenType) ==
                precedenceMap.end()) {
            return lhs;
        }
        auto precedenceMapEntry =
            precedenceMap.find((*tokenIt).get()->tokenType);
        auto nextPrecedence = precedenceMapEntry->second;
        while (
            (*tokenIt).get() != 0x0 && (*(tokenIt + 1)).get() != 0x0 &&
            !((((*(tokenIt + 1)).get()->tokenType) == TokenType::Semicolon)) &&
            precedence < nextPrecedence) {
            std::vector<TokenType> hasInfix = {TokenType::Plus,
                                               TokenType::Minus};
            if (std::find(hasInfix.begin(), hasInfix.end(),
                          (*tokenIt).get()->tokenType) != hasInfix.end()) {
                lhs = parseInfix(lhs);
            }
            return lhs;
        }
        return expressionNode;
    }

   private:
    std::vector<std::shared_ptr<Token>> tokens;
    std::vector<std::shared_ptr<Token>>::iterator tokenIt;

    std::shared_ptr<StatementNode> parseStatement() {
        std::shared_ptr<StatementNode> statement;
        switch ((*tokenIt).get()->tokenType) {
            case TokenType::Let: {
                auto letStatement = std::make_shared<LetStatement>();
                tokenIt = tokenIt + 1;  // move to the identifier
                assertToken(tokenIt, TokenType::Identifier);
                std::cout << "Token should be identifier: "
                          << (*tokenIt).get()->tokenType;
                std::string identifierString =
                    (*tokenIt).get()->identifierValue;

                tokenIt = tokenIt + 1;  // move to equal
                assertToken(tokenIt, TokenType::Equal);
                std::cout << "Token should be equal: "
                          << (*tokenIt).get()->tokenType;
                tokenIt = tokenIt + 1;  // move to value expression

                std::cout << "Parsing rvalue in let\n";
                std::cout << "Token should be number: "
                          << (*tokenIt).get()->tokenType << "\n";
                auto value = parseExpression(Precedence::Lowest);

                assertToken(tokenIt, TokenType::Semicolon);
                tokenIt++;

                letStatement->value = value;
                letStatement->identifier = identifierString;

                statement = std::make_shared<StatementNode>(letStatement);
                break;
            }
            case TokenType::Return: {
                assert(false && "Unimplemented");
            }
            default: {
                std::cout << "From statement " << (*tokenIt).get()->tokenType
                          << "\n";
                auto expressionNode = parseExpression(Precedence::Lowest);
                std::shared_ptr<ExpressionStatement> expressionStatement =
                    std::make_shared<ExpressionStatement>(expressionNode);
                statement =
                    std::make_shared<StatementNode>(expressionStatement);
                break;
            }
        }
        return statement;
    }

    std::shared_ptr<ExpressionNode> parsePrefix(Precedence precedence) {
        switch ((*tokenIt).get()->tokenType) {
            case TokenType::Number:
                return parseInteger(precedence);
            case TokenType::String:
                return parseString(precedence);
            case TokenType::Function: {
                return parseFunction(precedence);
            }
            case TokenType::Identifier: {
                return parseIdentifier(precedence);
            }
            default:
                std::cout << (*tokenIt).get()->tokenType << "\n";
                assert(false && "Should'nt hit here");
        }
    }
    std::shared_ptr<ExpressionNode> parseFunction(Precedence precedence) {
        tokenIt++;
        assertToken(tokenIt, TokenType::Identifier);
        auto name = (*tokenIt).get()->identifierValue;
        tokenIt++;
        auto arguments = parseFunctionArguments();
        auto body = parseFunctionBody();
        auto funcExpression =
            std::make_shared<FunctionExpression>(name, arguments, body);
        return std::make_shared<ExpressionNode>(funcExpression);
    }

    std::shared_ptr<BlockStatement> parseFunctionBody() {
        auto statements = std::vector<std::shared_ptr<StatementNode>>();
        assertToken(tokenIt, TokenType::LBrace);
        tokenIt++;
        while (true) {
            if ((*tokenIt).get()->tokenType == TokenType::Semicolon) {
                tokenIt++;
                continue;
            }
            if ((*tokenIt).get()->tokenType == TokenType::RBrace) {
                tokenIt++;
                break;
            }
            auto statement = parseStatement();
            statements.emplace_back(statement);
        }
        return std::make_shared<BlockStatement>(statements);
    }

    std::shared_ptr<std::vector<std::shared_ptr<ExpressionNode>>>
    parseFunctionArguments() {
        assertToken(tokenIt, TokenType::LParen);
        auto arguments =
            std::make_shared<std::vector<std::shared_ptr<ExpressionNode>>>();
        while (true) {
            // TODO: refactor
            // Check if after parsing expression RBrace is reached
            if ((*tokenIt).get()->tokenType == TokenType::RParen) {
                tokenIt++;
                break;
            }
            tokenIt++;
            if ((*tokenIt).get()->tokenType == TokenType::Comma) {
                tokenIt++;
                continue;
            }
            // For special case when there are no arguments
            if ((*tokenIt).get()->tokenType == TokenType::RParen) {
                tokenIt++;
                break;
            }
            arguments->emplace_back(parseExpression(Precedence::Lowest));
        }
        return arguments;
    }
    std::shared_ptr<ExpressionNode> parseInteger(Precedence precedence) {
        auto intExpression =
            std::make_shared<IntegerExpression>((*tokenIt).get()->numValue);
        tokenIt++;
        auto expressionNode = std::make_shared<ExpressionNode>(intExpression);
        return expressionNode;
    }

    std::shared_ptr<ExpressionNode> parseIdentifier(Precedence precedence) {
        auto identifierExpression = std::make_shared<IdentifierExpression>(
            (*tokenIt).get()->identifierValue);
        tokenIt++;
        auto expressionNode =
            std::make_shared<ExpressionNode>(identifierExpression);
        return expressionNode;
    }

    std::shared_ptr<ExpressionNode> parseString(Precedence precedence) {
        auto stringExpression =
            std::make_shared<StringExpression>((*tokenIt).get()->stringValue);
        tokenIt++;
        auto expressionNode =
            std::make_shared<ExpressionNode>(stringExpression);
        return expressionNode;
    }
    std::shared_ptr<ExpressionNode> parseInfix(
        std::shared_ptr<ExpressionNode> left) {
        switch ((*tokenIt).get()->tokenType) {
            case TokenType::Plus:
            case TokenType::Minus: {
                std::cout << "Parsing infix plus/minus"
                          << "\n";
                auto currentOp = (*tokenIt).get()->tokenType;
                tokenIt++;
                inMapAssert(precedenceMap, currentOp);
                auto rhs = parseExpression(
                    precedenceMap.find((*tokenIt).get()->tokenType)->second);
                auto infixExpression =
                    std::make_shared<InfixExpression>(left, currentOp, rhs);
                auto expressionNode =
                    std::make_shared<ExpressionNode>(infixExpression);
                return expressionNode;
            }
            default:
                std::cout << (*tokenIt).get()->tokenType << "\n";
                assert(false && "Should'nt hit this");
        }
    }
};