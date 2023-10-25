#pragma once
#include <algorithm>
#include <map>
#include <memory>
#include "lexer.h"

#define assertToken(tokenIt, type) assert((*tokenIt).get()->tokenType == type)
#define inMapAssert(map, key) assert(map.find(key) != map.end());

// Forward declarations in no particular order
class BlockStatement;
class ReturnStatement;
class ExpressionNode;
class StatementNode;

enum class NodeType {
    Program,
    Statement,
    Expression,
};

enum class StatementType {
    Let,
    Block,
    Return,
    Expression,
    Function,
    If,
    For,
    While
};

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

class IfStatement {
   public:
    std::shared_ptr<ExpressionNode> condition;
    std::shared_ptr<BlockStatement> trueBlock;
    std::shared_ptr<BlockStatement> falseBlock;
    IfStatement(std::shared_ptr<ExpressionNode> condition,
                std::shared_ptr<BlockStatement> trueBlock,
                std::shared_ptr<BlockStatement> falseBlock)
        : condition(condition), trueBlock(trueBlock), falseBlock(falseBlock) {}
};

class FunctionArg {
   public:
    TokenType type;
    std::shared_ptr<std::string> name;
    std::shared_ptr<ExpressionNode> value;
    FunctionArg(TokenType type,
                std::shared_ptr<ExpressionNode> value,
                std::shared_ptr<std::string> name)
        : type(type), value(value), name(name) {}
};

class FunctionStatement {
   public:
    std::string name;
    std::vector<std::shared_ptr<FunctionArg>> arguments;
    std::shared_ptr<BlockStatement> body;
    TokenType returnType;
    FunctionStatement() {}
    FunctionStatement(std::string name,
                      std::vector<std::shared_ptr<FunctionArg>> arguments,
                      std::shared_ptr<BlockStatement> body,
                      TokenType returnType)
        : name(name),
          arguments(arguments),
          body(body),
          returnType(returnType) {}
};

class IdentifierExpression {
   public:
    std::shared_ptr<std::string> name;
    IdentifierExpression() {}
    IdentifierExpression(std::shared_ptr<std::string> name) : name(name) {}
};

class CallExpression {
   public:
    std::shared_ptr<std::string> fnName;
    std::vector<std::shared_ptr<ExpressionNode>> arguments;
    CallExpression(std::shared_ptr<std::string> fnName,
                   std::vector<std::shared_ptr<ExpressionNode>> arguments)
        : fnName(fnName), arguments(arguments) {}
};

class ExpressionNode {
   public:
    ExpressionType type;
    std::shared_ptr<StringExpression> stringExp;
    std::shared_ptr<IntegerExpression> integerExp;
    std::shared_ptr<InfixExpression> infixExpr;
    std::shared_ptr<IdentifierExpression> identifierExpression;
    std::shared_ptr<CallExpression> callExpression;
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
    ExpressionNode(std::shared_ptr<IdentifierExpression> identifierExpression)
        : identifierExpression(identifierExpression),
          type(ExpressionType::Identifier) {}
    ExpressionNode(std::shared_ptr<CallExpression> callExpression)
        : callExpression(callExpression), type(ExpressionType::Call) {}
};

class LetStatement {
   public:
    std::shared_ptr<std::string> identifier;
    std::shared_ptr<ExpressionNode> value;
    LetStatement() {}
};

class BlockStatement {
   public:
    std::vector<std::shared_ptr<StatementNode>> statements;
    BlockStatement(std::vector<std::shared_ptr<StatementNode>> statements)
        : statements(statements) {}
};

class ReturnStatement {
   public:
    std::shared_ptr<ExpressionNode> returnValue;
    ReturnStatement(std::shared_ptr<ExpressionNode> returnValue)
        : returnValue(returnValue) {}
};

class ExpressionStatement {
   public:
    std::shared_ptr<ExpressionNode> expression;
    ExpressionStatement(std::shared_ptr<ExpressionNode> expression)
        : expression(expression) {}
};

class ForStatement {
   public:
    std::shared_ptr<StatementNode> initialCondition;
    std::shared_ptr<ExpressionNode> continueCondition;
    std::shared_ptr<StatementNode> updateCondition;
    std::shared_ptr<BlockStatement> body;
    ForStatement(std::shared_ptr<StatementNode> initialCondition,
                 std::shared_ptr<ExpressionNode> continueCondition,
                 std::shared_ptr<StatementNode> updateCondition,
                 std::shared_ptr<BlockStatement> body)
        : initialCondition(initialCondition),
          continueCondition(continueCondition),
          updateCondition(updateCondition),
          body(body) {}
};

class StatementNode {
   public:
    StatementType type;
    std::shared_ptr<LetStatement> letStatement;
    std::shared_ptr<ExpressionStatement> expressionStatement;
    std::shared_ptr<FunctionStatement> funcStatement;
    std::shared_ptr<ReturnStatement> returnStatement;
    std::shared_ptr<IfStatement> ifStatement;
    std::shared_ptr<ForStatement> forStatement;
    StatementNode() {}
    StatementNode(std::shared_ptr<LetStatement> letStatement)
        : type(StatementType::Let), letStatement(letStatement) {}
    StatementNode(std::shared_ptr<FunctionStatement> functionStatement)
        : type(StatementType::Function), funcStatement(functionStatement) {}
    StatementNode(std::shared_ptr<ReturnStatement> returnStatement)
        : type(StatementType::Return), returnStatement(returnStatement) {}
    StatementNode(std::shared_ptr<ExpressionStatement> expressionStatement) {
        this->type = StatementType::Expression;
        this->expressionStatement = expressionStatement;
    }
    StatementNode(std::shared_ptr<IfStatement> ifStatement)
        : ifStatement(ifStatement), type(StatementType::If) {}
    StatementNode(std::shared_ptr<ForStatement> forStatement)
        : forStatement(forStatement), type(StatementType::For) {}
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
    {TokenType::LParen, Precedence::Call},
    {TokenType::LessThan, Precedence::Lessgreater},
    {TokenType::GreaterThan, Precedence::Lessgreater},
    {TokenType::Equal, Precedence::Equals}};

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
            std::vector<TokenType> hasInfix = {
                TokenType::Plus,     TokenType::Minus,       TokenType::LParen,
                TokenType::LessThan, TokenType::GreaterThan, TokenType::Equal};
            if (std::find(hasInfix.begin(), hasInfix.end(),
                          (*tokenIt).get()->tokenType) != hasInfix.end()) {
                lhs = parseInfix(precedence, lhs);
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
                letStatement->identifier =
                    std::make_shared<std::string>(identifierString);

                statement = std::make_shared<StatementNode>(letStatement);
                break;
            }
            case TokenType::Return: {
                tokenIt++;
                auto returnValue = parseExpression(Precedence::Lowest);
                assertToken(tokenIt, TokenType::Semicolon);
                tokenIt++;
                auto returnStatement =
                    std::make_shared<ReturnStatement>(returnValue);
                statement = std::make_shared<StatementNode>(returnStatement);
                break;
            }
            case TokenType::Function: {
                statement = parseFunction();
                break;
            }
            case TokenType::If: {
                statement = parseIf();
                break;
            }
            case TokenType::For: {
                statement = parseFor();
                break;
            }
            default: {
                std::cout << "From statement " << (*tokenIt).get()->tokenType
                          << "\n";
                auto expressionNode = parseExpression(Precedence::Lowest);
                std::shared_ptr<ExpressionStatement> expressionStatement =
                    std::make_shared<ExpressionStatement>(expressionNode);
                if ((*tokenIt)->tokenType == TokenType::Semicolon) {
                    tokenIt++;  // TODO: refactor this hack
                }
                statement =
                    std::make_shared<StatementNode>(expressionStatement);
                break;
            }
        }
        return statement;
    }

    std::shared_ptr<StatementNode> parseFor() {
        tokenIt++;
        assertToken(tokenIt, TokenType::LParen);
        tokenIt++;
        auto initialCondition = parseStatement();
        auto continueCondition = parseExpression(Precedence::Lowest);
        assertToken(tokenIt, TokenType::Semicolon);
        tokenIt++;
        auto updateCondition = parseStatement();
        assertToken(tokenIt, TokenType::RParen);
        tokenIt++;
        auto body = parseBlockStatement();
        auto forStatement = std::make_shared<ForStatement>(
            initialCondition, continueCondition, updateCondition, body);
        return std::make_shared<StatementNode>(forStatement);
    }
    std::shared_ptr<StatementNode> parseIf() {
        tokenIt++;
        assertToken(tokenIt, TokenType::LParen);
        tokenIt++;
        auto condition = parseExpression(Precedence::Lowest);
        assertToken(tokenIt, TokenType::RParen);
        tokenIt++;
        auto trueBody = parseBlockStatement();
        assertToken(tokenIt, TokenType::Else);
        tokenIt++;
        auto falseBody = parseBlockStatement();
        auto ifStatement =
            std::make_shared<IfStatement>(condition, trueBody, falseBody);
        auto statement = std::make_shared<StatementNode>(ifStatement);
        return statement;
    }

    std::shared_ptr<ExpressionNode> parsePrefix(Precedence precedence) {
        switch ((*tokenIt).get()->tokenType) {
            case TokenType::Number:
                return parseInteger(precedence);
            case TokenType::String:
                return parseString(precedence);
            case TokenType::Identifier: {
                return parseIdentifier(precedence);
            }
            default:
                std::cout << (*tokenIt).get()->tokenType << "\n";
                assert(false && "Should'nt hit here");
        }
    }
    std::shared_ptr<StatementNode> parseFunction() {
        tokenIt++;
        assertToken(tokenIt, TokenType::Identifier);
        auto name = (*tokenIt).get()->identifierValue;
        tokenIt++;
        auto arguments = parseFunctionArguments();
        assertToken(tokenIt, TokenType::Colon);
        tokenIt++;
        // TODO:have an assert for the return types
        auto returnType = (*tokenIt)->tokenType;
        tokenIt++;
        auto body = parseBlockStatement();
        auto funcStatement = std::make_shared<FunctionStatement>(
            name, arguments, body, returnType);
        return std::make_shared<StatementNode>(funcStatement);
    }

    std::shared_ptr<BlockStatement> parseBlockStatement() {
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

    std::vector<std::shared_ptr<FunctionArg>> parseFunctionArguments() {
        assertToken(tokenIt, TokenType::LParen);
        auto arguments = std::vector<std::shared_ptr<FunctionArg>>();
        while (true) {
            // TODO: refactor
            // Check if after parsing expression RBrace is reached
            if ((*tokenIt).get()->tokenType == TokenType::RParen) {
                tokenIt++;
                break;
            }
            tokenIt++;
            if ((*tokenIt).get()->tokenType == TokenType::Comma) {
                continue;
            }
            // For special case when there are no arguments
            if ((*tokenIt).get()->tokenType == TokenType::RParen) {
                tokenIt++;
                break;
            }
            auto argExpression = parseExpression(Precedence::Lowest);
            assert(argExpression->type == ExpressionType::Identifier &&
                   "Should be an identifier");
            assertToken(tokenIt, TokenType::Colon);
            tokenIt++;
            auto type = (*tokenIt)->tokenType;
            auto functionArg = std::make_shared<FunctionArg>(
                type, argExpression, argExpression->identifierExpression->name);
            arguments.emplace_back(functionArg);
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
            std::make_shared<std::string>((*tokenIt).get()->identifierValue));
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
    std::shared_ptr<ExpressionNode> parseCallExpression(
        Precedence precedence,
        std::shared_ptr<ExpressionNode> left) {
        auto expressions = std::vector<std::shared_ptr<ExpressionNode>>();
        while ((*tokenIt)->tokenType != TokenType::RParen) {
            expressions.emplace_back(parseExpression(Precedence::Lowest));
            if ((*tokenIt)->tokenType == TokenType::Comma) {
                tokenIt++;
                continue;
            }
        }
        auto callExpression = std::make_shared<CallExpression>(
            left->identifierExpression->name, expressions);
        tokenIt++;
        return std::make_shared<ExpressionNode>(callExpression);
    }
    std::shared_ptr<ExpressionNode> parseInfix(
        Precedence precedence,
        std::shared_ptr<ExpressionNode> left) {
        switch ((*tokenIt).get()->tokenType) {
            case TokenType::Plus:
            case TokenType::Minus:
            case TokenType::LessThan:
            case TokenType::GreaterThan:
            case TokenType::Equal: {
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
            case TokenType::LParen: {
                tokenIt++;
                return parseCallExpression(precedence, left);
            }
            default:
                std::cout << (*tokenIt).get()->tokenType << "\n";
                assert(false && "Should'nt hit this");
        }
    }
};
