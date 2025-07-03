#include <iostream>
#include <vector>
#include <string>
#include <memory>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <cstdlib>
#include <variant>

using namespace std;

// ================ Token Kind ==================

/// @brief Which kind of Token is
enum class TokenType{

    // Variable Token

    Number,
    String,
    
    // Keywords Token

    Let,
    Input,
    PrintNewLine,
    Print,
    If,
    ElseIf,
    Else,
    While,
    For,
    Exit,
    End,
    Break,
    Continue,
    Fun,
    Return,

    // General Token

    Semicolon,
    EndOfLine,
    Identifier,
    DoubleQuotes,
    Comma,

    // Arithmatic Tokens

    Plus,
    Minus,
    Star,
    Slash,
    Mod,

    // unary token

    Not,

    // Assignment Tokens

    Equal,

    // increment - decrement token

    PlusPlus,
    MinusMinus,

    // Compare Token

    EqualEqual,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Logical Token

    And,
    Or,
    
    // Brackets Tokens

    Open_Brace,
    Close_Brace,
    Open_Parentheses,
    Close_Parentheses,

    // Unknown Tokens

    Unknown,
};

/// @brief This function is like util for debug that converts enum if TokenType into String so we can easy print it
/// @param type TokenType
/// @return TokenType into String
string TokenTypeToString(TokenType type) {
    switch (type) { 
        case TokenType::Number:               return "Number_Token";
        case TokenType::String:               return "String_Token";
        case TokenType::Semicolon:            return "Semicolon_Token";
        case TokenType::EndOfLine:            return "EndOfLine_Token";
        case TokenType::EqualEqual :          return "EqualEqual_Token";
        case TokenType::NotEqual :            return "NotEqual_Token";
        case TokenType::Less :                return "Less_Token";
        case TokenType::LessEqual :           return "LessEqual_Token";
        case TokenType::Greater :             return "Greater_Token";        
        case TokenType::GreaterEqual :        return "GreaterEqual_Token";
        case TokenType::PlusPlus :            return "PlusPlus_Token";
        case TokenType::MinusMinus :          return "MinusMinus_Token";
        case TokenType::While :               return "While_Token";
        case TokenType::For :                 return "For_Token";
        case TokenType::Comma :               return "Comma_Token";
        case TokenType::Break :               return "Break_Token";
        case TokenType::Continue :            return "Continue_Token";
        case TokenType::DoubleQuotes:         return "DoubleQuotes_Token";
        case TokenType::Not:                  return "Not_Token";
        case TokenType::Plus:                 return "Plus_Token";
        case TokenType::Minus:                return "Minus_Token";
        case TokenType::Star:                 return "Star_Token";
        case TokenType::Slash:                return "Slash_Token";
        case TokenType::Mod:                  return "Mod_Token";
        case TokenType::Equal:                return "Equal_Token";
        case TokenType::And:                  return "And_Token";
        case TokenType::Or:                   return "Or_Token";
        case TokenType::Exit:                 return "Exit_Token";
        case TokenType::End:                  return "End_Token";
        case TokenType::Input:                return "Input_Token";
        case TokenType::Print:                return "Print_Token";
        case TokenType::PrintNewLine:         return "PrintNewLine_Token";
        case TokenType::Identifier:           return "Identifier_Token";
        case TokenType::Let:                  return "Let_Token";
        case TokenType::If:                   return "If_Token";
        case TokenType::ElseIf:               return "ElseIf_Token";
        case TokenType::Else:                 return "Else_Token";
        case TokenType::Fun:                  return "Fun_Token";
        case TokenType::Return:               return "Return_Token";
        case TokenType::Open_Brace:           return "Open_Brace_Token";
        case TokenType::Close_Brace:          return "Close_Brace_Token";
        case TokenType::Open_Parentheses:     return "Open_Parentheses_Token";
        case TokenType::Close_Parentheses:    return "Close_Parentheses_Token";
        case TokenType::Unknown:              return "Unknown_Token";
        default:                              return "Error : UnknownType";
    }
}

// ================ Token Structure ==================

/// @brief This is structure for token
struct Token
{
    int position;
    TokenType tokenType;
    string token;
    int value;


    /// @brief Debug Util : prints given Token
    /// @param token Token
    void printTokens(){  
        cout<< position << ") "<<TokenTypeToString(tokenType) << " : " << token << endl;
    }
};

// ================ Lexer ==================

/// @brief For converting Input into Tokens
class Lexer{

private:
    vector<Token> Tokens;
    int pos = 0;
    Token token;
    string Input;

    unordered_map<char,TokenType> singleCharToken = {
        {'+', TokenType::Plus},
        {'-', TokenType::Minus},
        {'*', TokenType::Star},
        {'/', TokenType::Slash},
        {'%', TokenType::Mod},
        {'=', TokenType::Equal},
        {'{', TokenType::Open_Brace},
        {'}', TokenType::Close_Brace},
        {'(', TokenType::Open_Parentheses},
        {')', TokenType::Close_Parentheses},
        {';', TokenType::Semicolon},
        {'<', TokenType::Less},  
        {'>', TokenType::Greater},
        {'!', TokenType::Not},          
        {',', TokenType::Comma},  
    };

    unordered_map<string,TokenType> doubleCharToken = {
        {"==",TokenType::EqualEqual},
        {"!=",TokenType::NotEqual},
        {"<=",TokenType::LessEqual},
        {">=",TokenType::GreaterEqual},
        {"++",TokenType::PlusPlus},
        {"--",TokenType::MinusMinus},
        {"&&",TokenType::And},
        {"||",TokenType::Or}
    };

    unordered_map<string,TokenType> keywordToken = {
        {"laile",   TokenType::Let},
        {"input",   TokenType::Input},
        {"println", TokenType::PrintNewLine},
        {"print",   TokenType::Print},
        {"if",      TokenType::If},
        {"elif",    TokenType::ElseIf},
        {"else",    TokenType::Else},
        {"while",   TokenType::While},
        {"for",     TokenType::For},
        {"contine", TokenType::Continue},
        {"break",   TokenType::Break},
        {"fun",     TokenType::Fun},
        {"return",  TokenType::Return},
        {"exit",    TokenType::Exit},
        {"end",     TokenType::End}
    };


    // Return the current character with optional lookahead
    char peek(int offset = 0) {
        if ((pos + offset) >= Input.size()) {
            return '\0';  // Safe EOF sentinel
        }
        return Input[pos + offset];
    }

    // Advance the position and return the current character
    char consume(int offset = 1) {
        if ((pos + offset - 1) >= Input.size()) {
            cerr << "Error: Tried to consume past end of input at pos = " << pos 
                 << ", offset = " << offset << ", size = " << Input.size() << endl;
            throw runtime_error("Lexer::consume out of bounds");
        }
        char character = Input[pos];
        pos += offset;
        return character;
    }


    // check's first letter of identifier
    bool checkIdentifierStart(char ch){
        return isalpha(ch) || ch == '_';
    }

    bool checkIdentifierBody(char ch){
        return isalnum(ch) || ch == '_';
    }
public:

    Lexer(string input) : Input(move(input)){
        // if needed add later
    }

    /// @brief This method reads the input char by char and converts into whole integer
    /// @return Convert char by char digits into whole integer
    int parseInteger(){
        int result = 0;
        while(isdigit(peek())){
            result = result * 10 + (consume() - '0');
        }
        return result;
    }

    /// @brief This function is for converting string into each tokens
    /// @return Vector of Tokens
    vector<Token> Tokenizer(){
        while(peek() != '\0'){
            char ch = peek();
            string twoChar = string() + peek() + peek(1);
            if(isdigit(ch)){
                int _pos = pos;
                int value = parseInteger();
                Tokens.push_back({_pos,TokenType::Number,to_string(value),value});
            }
            else if (doubleCharToken.count(twoChar)){
                Tokens.push_back({pos,doubleCharToken[twoChar],twoChar,0});
                //consume(); // first char
                consume(2); // two char
            }
            else if(singleCharToken.count(ch)){
                Tokens.push_back({pos,singleCharToken[ch],string(1,ch),0});         
                consume();       
            }
            else if (ch == '"') {
                std::string strValue;
                consume();

                while (pos < Input.length() && peek() != '"') {
                    strValue += consume();
                }
            
                if (pos >= Input.length() || peek() != '"') {
                    throw std::runtime_error("Unterminated string literal");
                }
            
                consume();
            
                Tokens.push_back({pos,TokenType::String,strValue,0});
            }
            else if(checkIdentifierStart(ch)){
                int _pos = pos;
                string id;
                while (checkIdentifierBody(peek())){
                    id += consume();
                }
                if(keywordToken.count(id)){
                    Tokens.push_back({_pos,keywordToken[id],id,0});
                }else{
                    Tokens.push_back({_pos,TokenType::Identifier,id,0});
                }
            }

            else if(isspace(ch)){
                consume();            
            }

            else{
                Tokens.push_back({pos,TokenType::Unknown,string(1,ch),0});
                cerr<<"Error : Unexpected token" << ch << " position " << pos <<endl;
                consume();
            }
        }
        Tokens.push_back({pos,TokenType::EndOfLine,"EOfL",0});
        return Tokens;
    }
};


// ================ AST Node Stuff ==================

/// @brief base of ast node
struct ASTNode{
    virtual ~ASTNode() {};
    virtual void print() const = 0;
};

/// @brief ast node that stores number
struct NumberNode : ASTNode{
    int value;
    NumberNode(int val) : value(val){};
    void print() const override {cout<<value;}
};

struct StringNode : ASTNode{
    string value;
    StringNode(const string& val) : value(val){};
    void print() const override {cout<<value;}
};

/// @brief ast node that stores one binary expression
struct BinaryOperatorNode : ASTNode{
    string op;
    unique_ptr<ASTNode> left;
    unique_ptr<ASTNode> right;

    BinaryOperatorNode(unique_ptr<ASTNode> l,string o,unique_ptr<ASTNode> r) : left(move(l)),op(move(o)),right(move(r)) {};

    void print() const override{
        cout<< "( "; 
        left->print();
        cout<< " " << op << " ";
        right->print();
        cout << " )";
    }
};

struct UnaryOperatorNode : ASTNode {
    string op;
    unique_ptr<ASTNode> operand;

    UnaryOperatorNode(string o,unique_ptr<ASTNode> val): op(move(o)),operand(move(val)){}

    void print() const override {
        cout << "(" << op;
        operand->print();
        cout << ")";
    }
};

/// @brief ast node thar stores variable name
struct VariableNameNode : ASTNode{
    string name;
    VariableNameNode(const string& n) : name(n) {}
    void print() const override{
        cout<<name;
    } 
};

/// @brief stores whole varible with value
struct AssignmentNode : ASTNode{
    string variableName;
    unique_ptr<ASTNode> value;
    AssignmentNode(const string& name,unique_ptr<ASTNode> val): variableName(name),value(move(val)){};
    void print() const override{
        cout << variableName << " = ";
        value->print();
    }
};

struct ExitNode : ASTNode{
    unique_ptr<ASTNode> value;
    ExitNode(unique_ptr<ASTNode> val) : value(move(val)){};
    void print() const override {
        cout<<"exit ( ";
        value->print();
        cout<<" )";
    }
};

struct PrintNode : ASTNode{
    unique_ptr<ASTNode> value;
    PrintNode(unique_ptr<ASTNode> val): value(move(val)){};
    void print() const override {
        cout << ">  ";
        value->print();
    }
};
struct PrintNewLineNode : ASTNode{
    unique_ptr<ASTNode> value;
    PrintNewLineNode(unique_ptr<ASTNode> val): value(move(val)){};
    void print() const override {
        cout << ">  ";
        value->print();
    }
};
struct EndNode : ASTNode{
    void print() const override {
        cout<<"-------- code ended ----------";
    }
};

struct BreakNode : ASTNode {
    void print() const override {
        cout << "break" << endl;
    }
};

struct ContinueNode : ASTNode{
    void print() const override {
        cout << "continue" << endl;
    }
};


struct IfNode : ASTNode{
    vector<pair<unique_ptr<ASTNode>,vector<unique_ptr<ASTNode>>>> conditionalBlock;
    vector<unique_ptr<ASTNode>> elseBlock;

    IfNode(vector<pair<unique_ptr<ASTNode>,vector<unique_ptr<ASTNode>>>> condiBlock,vector<unique_ptr<ASTNode>> elseBlk):
        conditionalBlock(move(condiBlock)),elseBlock(move(elseBlk)){};
    
    void print() const override {
        for(const auto& [cond,block] : conditionalBlock){
            cout << "if (";
            cond->print();
            cout << ") {";
            for (const auto& stmt : block) stmt->print();
            cout << " }";
        }
        if(!elseBlock.empty()){
            cout << " else { ";
            for (const auto& stmt : elseBlock) stmt->print();
            cout << " }";
        }
    }
};

struct WhileNode : ASTNode{
    unique_ptr<ASTNode> condition;
    vector<unique_ptr<ASTNode>> body;
    WhileNode(unique_ptr<ASTNode> cond, vector<unique_ptr<ASTNode>> b):
        condition(move(cond)),body(move(b)){}

    void print() const override {
        cout<< "while (";
        condition->print();
        cout<<") { ";
        for (const auto& stmt : body) stmt->print();
        cout<<" }";
    }
};

struct ForNode : ASTNode{
    unique_ptr<ASTNode> initialization, condition, update;
    vector<unique_ptr<ASTNode>> body;

    ForNode(unique_ptr<ASTNode> init, unique_ptr<ASTNode> condi, unique_ptr<ASTNode> up, vector<unique_ptr<ASTNode>> b)
        : initialization(move(init)), condition(move(condi)), update(move(up)), body(move(b)){}

    void print() const override {
        cout << "for (";
        initialization->print();
        cout << "; ";
        condition->print();
        cout << "; ";
        update->print();
        cout << ") { ";
        for (auto& stmt : body) stmt->print();
        cout << " }";
    }
};

enum class IncDecType {PreIncrement, PostIncrement, PreDecrement, PostDecrement};

struct IncDecNode : ASTNode {
    string varName;
    IncDecType type;

    IncDecNode(const string& name, IncDecType t): varName(name),type(t){}

    void print() const override {
        if (type == IncDecType::PreIncrement) cout << "++" << varName;
        else if (type == IncDecType::PostIncrement) cout << varName << "++" ;
        else if (type == IncDecType::PreDecrement) cout << "--" << varName;
        else cout << varName << "--" ;
    }
};

struct FunctionNode : ASTNode {
    string name;
    vector<string> parameters; 
    vector<unique_ptr<ASTNode>> body;

    FunctionNode(string n, vector<string> parm, vector<unique_ptr<ASTNode>> b) 
        : name(move(n)),parameters(move(parm)),body(move(b)) {}
    
    void print() const override {
        cout << "fun " << name << "( ";
        for (const auto& p : parameters) cout << p << ",";
        cout << ") { ";
        for (auto& stmt : body) stmt->print();
        cout << " }" << endl;
    }
};

struct FunctionCallNode : ASTNode {       
    string name;
    vector<unique_ptr<ASTNode>> arguments;
    
    FunctionCallNode(string n, vector<unique_ptr<ASTNode>> args) : name(move(n)),arguments(move(args)) {}

    void print() const override {
        cout << name << "(";
        for (const auto& p : arguments) p->print();
        cout << ")"  << endl;
    }
};  

struct ReturnNode : ASTNode {
    unique_ptr<ASTNode> value;

    ReturnNode(unique_ptr<ASTNode> v = nullptr): value(move(v)){}

    void print() const override {
        cout << "return ";
        if (value) value->print();
    }
};

struct InputNode : ASTNode {
    void print() const override {
        cout << "input()";
    }
};



// ================ Parser ==================

/// @brief This is For converting or parsing tokens into ast (Abtract Syntax Tree)
class Parser{
private:

    unique_ptr<Lexer> lexer;
    vector<Token> Tokens;
    enum class valueType{ Number, String};
    static inline unordered_map<string, valueType> variableType;
    static inline unordered_map<string, string> stringStorage;
    static inline unordered_map<string, int64_t> variableOffsets;
    static inline unordered_map<string, FunctionNode*> functionTable;
    int currentStackOffset = 0;
    int pos = 0;
    int data = 4;

    int allocateVariable(const string& name) {
        currentStackOffset -= 64;
        variableOffsets[name] = currentStackOffset;
        return currentStackOffset;
    }


    /// @brief For accessing next token
    /// @return Next Token
    Token peek(int offset = 0){
        if (pos + offset >= Tokens.size()) {
            return {pos + offset, TokenType::EndOfLine, "", 0};
        }
        return Tokens[pos + offset];
    }

    /// @brief consumes current token and increments the position
    /// @return A previous token 
    Token consume(){
        if(pos < Tokens.size()) pos++;
        return Tokens[pos - 1];
    }
    /// @brief For comparing the given tokentype with current position token
    /// @param expected compare givens current token 
    /// @return true or false based on if matched or not
    bool match(TokenType expected){
        if(peek().tokenType == expected){
            consume();
            return true;
        }
        return false;
    }

    unique_ptr<ASTNode> parseStatement(){
        if(peek().tokenType == TokenType::Exit){
            consume(); // exit token
            if(!match(TokenType::Open_Parentheses)){
                cerr << "Syntax Error: expected '(' after 'exit'" << endl;
                return nullptr;
            }
            auto expr = parseExpression();

            if(!match(TokenType::Close_Parentheses)){
                cerr << "Syntax Error: expected ')' after exit expression" << endl;
                return nullptr;
            }

            return make_unique<ExitNode>(unique_ptr<ASTNode>(expr.release()));
        }
        else if(match(TokenType::Fun)){
            string functionName = consume().token;
            if(!match(TokenType::Open_Parentheses)) {
                cerr << "Error: expected ( after function name" << endl;
                return {};
            }
            vector<string> parm;
            while(!match(TokenType::Close_Parentheses)){
                if(peek().tokenType == TokenType::Identifier){
                    parm.push_back(consume().token);
                    match(TokenType::Comma);
                }
                else{
                    cerr << "Expected parameter name" << endl;
                    exit(1);
                }
            }

            if(!match(TokenType::Open_Brace)) {
                cerr << "Error: expected { after function declaration" << endl;
                return {};
            }
            vector<unique_ptr<ASTNode>> body;
            while (!match(TokenType::Close_Brace)){
                auto stmt = parseStatement();
                if(!stmt) return {};
                bool isBlockish =
                    dynamic_cast<IfNode*>(stmt.get()) ||
                    dynamic_cast<WhileNode*>(stmt.get()) ||
                    dynamic_cast<ForNode*>(stmt.get()) ||
                    dynamic_cast<FunctionNode*>(stmt.get()) ||
                    dynamic_cast<EndNode*>(stmt.get());

                if (!isBlockish && !match(TokenType::Semicolon)) {
                    cerr << "Expected ';' inside block" << endl;
                    return nullptr;
                }
                body.push_back(move(stmt));
            }
            return make_unique<FunctionNode>(functionName,move(parm),move(body));
        }
        else if(match(TokenType::Return)){
            if (peek().tokenType == TokenType::Semicolon || peek().tokenType == TokenType::Close_Brace){
                return make_unique<ReturnNode>();
            }
            auto expr = parseLogical();
            return make_unique<ReturnNode>(move(expr));
        }
        else if(((peek().tokenType == TokenType::PlusPlus || peek().tokenType == TokenType::MinusMinus) && peek(1).tokenType == TokenType::Identifier)){
            TokenType op = consume().tokenType;
            string var = consume().token;
            IncDecType type = (op == TokenType::PlusPlus) ? IncDecType::PreIncrement : IncDecType::PreDecrement;
            return make_unique<IncDecNode>(var,type);
        }
        else if((peek().tokenType == TokenType::Identifier && (peek(1).tokenType == TokenType::PlusPlus || peek(1).tokenType == TokenType::MinusMinus))){
            string var = consume().token;
            TokenType op = consume().tokenType;
            IncDecType type = (op == TokenType::PlusPlus) ? IncDecType::PostIncrement : IncDecType::PostDecrement;
            return make_unique<IncDecNode>(var,type);
        }
        else if(peek().tokenType == TokenType::PrintNewLine){
            consume(); // print token
            unique_ptr<ASTNode> expr;
            if(!match(TokenType::Open_Parentheses)){
                cerr << "Syntax Error: expected '(' after 'print'" << endl;
                return nullptr;
            }
            if(peek().tokenType == TokenType::String){
                string str = consume().token;
                expr = make_unique<StringNode>(str);
            }else{
                expr = parseExpression();
            }

            if(!match(TokenType::Close_Parentheses)){
                cerr << "Syntax Error: expected ')' after print expression" << endl;
                return nullptr;
            }
            return make_unique<PrintNewLineNode>(move(expr));
        }
        else if(peek().tokenType == TokenType::Print){
            consume(); // print token
            unique_ptr<ASTNode> expr;
            if(!match(TokenType::Open_Parentheses)){
                cerr << "Syntax Error: expected '(' after 'print'" << endl;
                return nullptr;
            }

            if(peek().tokenType == TokenType::String){
                
                string str = consume().token;
                expr = make_unique<StringNode>(str);
            }else{
                expr = parseExpression();
            }

            if(!match(TokenType::Close_Parentheses)){
                cerr << "Syntax Error: expected ')' after print expression" << endl;
                return nullptr;
            }
            return make_unique<PrintNode>(move(expr));
        }
        else if(peek().tokenType == TokenType::Let && peek(1).tokenType == TokenType::Identifier && peek(2).tokenType == TokenType::Equal){
            consume(); // let token
            string variableName = consume().token;
            consume(); // =
            unique_ptr<ASTNode> expr;
            if(peek().tokenType == TokenType::String){
                string str = consume().token;
                expr = make_unique<StringNode>(str);
                variableType[variableName] = valueType::String;
            }else{
                expr = parseExpression();
                allocateVariable(variableName);
                variableType[variableName] = valueType::Number;
            }
            return make_unique<AssignmentNode>(variableName,move(expr));
        }
        else if (peek().tokenType == TokenType::Identifier && peek(1).tokenType == TokenType::Equal){
            string variableName = consume().token;
            consume(); // = 

            unique_ptr<ASTNode> expr;
            if(peek().tokenType == TokenType::String){
                string str = consume().token;
                expr = make_unique<StringNode>(str);
            }
            else{
                expr = parseExpression();
            }

            if(variableType.find(variableName) == variableType.end()){
                cerr << "Warning : assigning to undeclared variable '" << variableName << "'\n";
                // or we can just allocate 
                allocateVariable(variableName);
                variableType[variableName] = valueType::Number;
            }

            return make_unique<AssignmentNode>(variableName,move(expr));
        }
        else if (peek().tokenType == TokenType::While){
            return parseWhileStatement();
        }
        else if (peek().tokenType == TokenType::For){
            return parseForStatement();
        }
        else if (peek().tokenType == TokenType::If){
            return parseIfStatement();
        }
        else if (match(TokenType::Break)){
            return make_unique<BreakNode>();
        }
        else if (match(TokenType::Continue)){
            return make_unique<ContinueNode>();
        }
        else if(peek().tokenType == TokenType::End){
            consume();
            return make_unique<EndNode>();
        }
        return parseExpression();
    }

    unique_ptr<ASTNode> parseLogical(){
        auto node = parseExpression();
        while(node){
            if(match(TokenType::And)){
                auto right = parseExpression();
                node = make_unique<BinaryOperatorNode>(move(node),"&&",move(right));
            }
            else if (match(TokenType::Or)){
                auto right = parseExpression();
                node = make_unique<BinaryOperatorNode>(move(node),"||",move(right));
            }
            else break;
        }
        return node;
    }

    /// @brief for making + and - token into binary expression
    /// @return a full binary expression with operator + or -
    unique_ptr<ASTNode> parseExpression(){
        auto node = parseTerm();
        while(true){

            TokenType tt = peek().tokenType;

            if (tt == TokenType::EqualEqual || tt == TokenType::NotEqual || tt == TokenType::Less || tt == TokenType::LessEqual ||
                tt == TokenType::Greater || tt == TokenType::GreaterEqual || tt == TokenType::Plus || tt == TokenType::Minus){

                string op = peek().token;
                consume();
                auto right = parseTerm();
                node = make_unique<BinaryOperatorNode>(unique_ptr<ASTNode>(node.release()),op,unique_ptr<ASTNode>(right.release()));
            
            }else break;
        }
        return node;
    }

    /// @brief for making * and / token into binary expression
    /// @return a full binary expression with operator * or /
    unique_ptr<ASTNode> parseTerm(){
        auto node = parseFactor();
        while(peek().tokenType == TokenType::Star || peek().tokenType == TokenType::Slash || peek().tokenType == TokenType::Mod){
            string op = peek().token;
            consume();
            auto right = parseFactor();
            node = make_unique<BinaryOperatorNode>(unique_ptr<ASTNode>(node.release()),op,unique_ptr<ASTNode>(right.release()));
        }
        return node;
    }
    

    /// @brief used for making or calling different function and making valid ast
    /// @return a number node or whole binary expression
    unique_ptr<ASTNode> parseFactor(){
        if(((peek().tokenType == TokenType::PlusPlus || peek().tokenType == TokenType::MinusMinus) && peek(1).tokenType == TokenType::Identifier)){
            TokenType op = consume().tokenType;
            string var = consume().token;
            IncDecType type = (op == TokenType::PlusPlus) ? IncDecType::PreIncrement : IncDecType::PreDecrement;
            return make_unique<IncDecNode>(var,type);
        }
        else if((peek().tokenType == TokenType::Identifier && (peek(1).tokenType == TokenType::PlusPlus || peek(1).tokenType == TokenType::MinusMinus))){
            string var = consume().token;
            TokenType op = consume().tokenType;
            IncDecType type = (op == TokenType::PlusPlus) ? IncDecType::PostIncrement : IncDecType::PostDecrement;
            return make_unique<IncDecNode>(var,type);
        }
        else if(peek().tokenType == TokenType::Identifier && peek(1).tokenType == TokenType::Open_Parentheses) {
            string name = consume().token;
            match(TokenType::Open_Parentheses);
            vector<unique_ptr<ASTNode>> args;
            if (!match(TokenType::Close_Parentheses)){
                while(true){
                    args.push_back(parseLogical());
                    if (match(TokenType::Close_Parentheses)) break;
                    if (!match(TokenType::Comma)){
                        cerr << "Expected ',' between arguments";
                        break;
                    }
                }
            }
            return make_unique<FunctionCallNode>(name,move(args));
        }
        else if(peek().tokenType == TokenType::Minus){
            consume(); // - 

            if(peek().tokenType == TokenType::Number){
                int val = -consume().value;
                return make_unique<NumberNode>(val);
            }

            auto operand = parseFactor();
            return make_unique<UnaryOperatorNode>("-",move(operand));
        }
        else if(peek().tokenType == TokenType::Not){
            consume(); // !

            if (match(TokenType::Open_Parentheses)) {
                auto expr = parseLogical();
                if (!match(TokenType::Close_Parentheses)) {
                    cerr << "Expexted ')' after !(...) at position " << pos << endl;
                    throw runtime_error("Error: unmatched ')'");
                }
                return make_unique<UnaryOperatorNode>("!",move(expr));
            }

            auto operand = parseFactor();
            return make_unique<UnaryOperatorNode>("!",move(operand));
        }
        else if(match(TokenType::Input)){
            if(!match(TokenType::Open_Parentheses) || !match(TokenType::Close_Parentheses)){
                cerr << "Syntax Error: expected '()' atfer input\n";
                exit(1);
            }
            return make_unique<InputNode>();
        }
        else if(match(TokenType::Number)){
            return make_unique<NumberNode>(Tokens[pos - 1].value);
        }
        else if (match(TokenType::Identifier)) {
            return make_unique<VariableNameNode>(Tokens[pos - 1].token);
        }
        else if(match(TokenType::Open_Parentheses)){
            auto node = parseExpression();
            if(!match(TokenType::Close_Parentheses)){
                cerr<<"Error : expected ')' at position "<< pos << endl;
            }
            return node;
        }
        else if(peek().tokenType == TokenType::And || peek().tokenType == TokenType::Or){
            return parseLogical();
        }
        else{
            cerr << "Unexpected token at position " << pos << " : " << peek().token << endl;
            throw runtime_error("Error: Unexpected token");
        }
    }

    /// @brief use for parsing if,else and else-if statement 
    /// @return a if node containing condition ,then block and else block
    unique_ptr<ASTNode> parseIfStatement(){
        vector<pair<unique_ptr<ASTNode>,vector<unique_ptr<ASTNode>>>> conditionalBlock; // (conditon,thenBloak)
        vector<unique_ptr<ASTNode>> elseBlock;
        
        auto parseBlock = [&](){
            vector<unique_ptr<ASTNode>> block;
            while(!match(TokenType::Close_Brace)){
                auto stmt = parseStatement();
                if (!stmt){
                    cerr << "Expected ';' inside if block" << endl;
                    return vector<unique_ptr<ASTNode>>();
                }

                bool isBlockish =
                    dynamic_cast<IfNode*>(stmt.get()) ||
                    dynamic_cast<WhileNode*>(stmt.get()) ||
                    dynamic_cast<ForNode*>(stmt.get()) ||
                    dynamic_cast<FunctionNode*>(stmt.get()) ||
                    dynamic_cast<EndNode*>(stmt.get());

                if (!isBlockish && !match(TokenType::Semicolon)) {
                    cerr << "Expected ';' inside block" << endl;
                    return vector<unique_ptr<ASTNode>>();
                }

                block.push_back(move(stmt));
            }
            return block;
        };

        auto parseConditionAndBlock = [&]() -> pair<unique_ptr<ASTNode>,vector<unique_ptr<ASTNode>>> {
            if(!match(TokenType::Open_Parentheses)){
                cerr<<"Expected '(' after 'if'" << endl;
                return {};
            }
            auto condi = parseLogical();
            if(!match(TokenType::Close_Parentheses)){
                cerr<<"Expected ')' after 'condition'" << endl;
                return {};
            }
            if(!match(TokenType::Open_Brace)){
                cerr<<"Expected '{' to start if block" << endl;
                return {};
            }
            auto block = parseBlock();
            return {move(condi),move(block)};    
        };
        
        consume(); // if token
        conditionalBlock.push_back(parseConditionAndBlock());
        while(match(TokenType::ElseIf)) {
            conditionalBlock.push_back(parseConditionAndBlock());
        }
        if (match(TokenType::Else)){
            if(!match(TokenType::Open_Brace)){
                cerr << "Expected '{' after 'else'" << endl;
                return nullptr;
            }
            elseBlock = parseBlock();
        }
        return make_unique<IfNode>(move(conditionalBlock),move(elseBlock));
    }

    unique_ptr<ASTNode> parseWhileStatement(){
        consume(); // while 
        if(!match(TokenType::Open_Parentheses)){
            cerr<<"Expected '(' after 'while'" << endl;
            return {};
        }
        auto condition = parseExpression();
        if(!match(TokenType::Close_Parentheses)){
            cerr<<"Expected ')' after 'condition'" << endl;
            return {};
        }
        if(!match(TokenType::Open_Brace)){
            cerr<<"Expected '{' to start while block" << endl;
            return {};
        }

        vector<unique_ptr<ASTNode>> body;
        while(!match(TokenType::Close_Brace)){
            auto stmt = parseStatement();
            if (!stmt){
                cerr << "Exprected ';' inside while loop" <<endl;
                return nullptr;
            }

            bool isBlockish =
                dynamic_cast<IfNode*>(stmt.get()) ||
                dynamic_cast<WhileNode*>(stmt.get()) ||
                dynamic_cast<ForNode*>(stmt.get()) ||
                dynamic_cast<FunctionNode*>(stmt.get()) ||
                dynamic_cast<EndNode*>(stmt.get());

            if (!isBlockish && !match(TokenType::Semicolon)) {
                cerr << "Expected ';' inside block" << endl;
                return nullptr;
            }

            body.push_back(move(stmt));
        }
        return make_unique<WhileNode>(move(condition),move(body));
    }

    unique_ptr<ASTNode> parseForStatement(){
        consume(); // for
        if(!match(TokenType::Open_Parentheses)){
            cerr<<"Expected '(' after 'while'" << endl;
            return {};
        }
        auto initialization = parseStatement();
        if(!match(TokenType::Semicolon)){
            cerr<<"Expected ';' after 'for initialization'" << endl;
            return {};
        }
        auto condition = parseExpression();
        if(!match(TokenType::Semicolon)){
            cerr<<"Expected ';' after 'for condition'" << endl;
            return {};
        }
        auto update = parseStatement();
        if(!match(TokenType::Close_Parentheses)){
            cerr<<"Expected ')' after 'condition'" << endl;
            return {};
        }
        if(!match(TokenType::Open_Brace)){
            cerr<<"Expected '{' to start while block" << endl;
            return {};
        }

        vector<unique_ptr<ASTNode>> body;
        while(!match(TokenType::Close_Brace)){
            auto stmt = parseStatement();
            if (!stmt){
                cerr << "Exprected ';' inside while loop" <<endl;
                return nullptr;
            }

            bool isBlockish =
                dynamic_cast<IfNode*>(stmt.get()) ||
                dynamic_cast<WhileNode*>(stmt.get()) ||
                dynamic_cast<ForNode*>(stmt.get()) ||
                dynamic_cast<EndNode*>(stmt.get());

            if (!isBlockish && !match(TokenType::Semicolon)) {
                cerr << "Expected ';' inside block" << endl;
                return nullptr;
            }

            body.push_back(move(stmt));
        }
        return make_unique<ForNode>(move(initialization),move(condition),move(update),move(body));        
    }


    string escapeString(string str){
        string result;
        string current;

        for (size_t i = 0; i < str.length(); ++i) {
            if (str[i] == '\\' && i + 1 < str.size()) {
                // Flush current chunk if not empty
                if (!current.empty()) {
                    if (!result.empty()) result += ", ";
                    result += "'" + current + "'";
                    current.clear();
                }

                char next = str[i + 1];
                if (next == 'n') {
                    if (!result.empty()) result += ", ";
                    result += "0x0A";
                    i++;
                } else if (next == 't') {
                    if (!result.empty()) result += ", ";
                    result += "0x09";
                    i++;
                } else if (next == '\\') {
                    current += '\\';
                    i++;
                } else if (next == '\'') {
                    current += '\'';
                    i++;
                } else if (next == '\"') {
                    current += '\"';
                    i++;
                } else {
                    current += '\\';
                }
            } else {
                current += str[i];
            }
        }

        // Add leftover chunk
        if (!current.empty()) {
            if (!result.empty()) result += ", ";
            result += "'" + current + "'";
        }
        return result;
    }

    bool isFunRetValue(FunctionNode* fun){
        for (auto& stmt : fun->body){
            if(auto* ret = dynamic_cast<ReturnNode*>(stmt.get())){
                if (ret->value) return true;
            }
        }
        return false;
    }

    vector<string> continueLabel;
    vector<string> breakLabel;
    string currentFunctionEndLabel = "";
    
    public:
    vector<string> assemblyCode;

    /// @brief for initiating tokens
    /// @param input code what convert
    Parser(string input){
        lexer = make_unique<Lexer>(input);
        Tokens = lexer->Tokenizer();
        assemblyCode.clear();

        assemblyCode.push_back("section .bss");
        assemblyCode.push_back("    buffer resb 32");
        assemblyCode.push_back("section .data");
        assemblyCode.push_back("    newline db 10");
        assemblyCode.push_back("section .text");
        assemblyCode.push_back("global _start");
        assemblyCode.push_back("_start:");
        assemblyCode.push_back("    push rbp");
        assemblyCode.push_back("    mov rbp, rsp");
        assemblyCode.push_back("    sub rsp, 1024");


        // Debug token output
        {   
            cout << "=== Tokens ===" << endl;
            int i = 0;
            for (auto& token : Tokens) {
                cout << i++ << "-";
                token.printTokens();
            }
            cout << "==============\n\n\n";
        }
    }
    /// @brief for actual parsing tokens into expression
    /// @return whole ast
    vector<unique_ptr<ASTNode>> parse(){

        vector<unique_ptr<ASTNode>> lines;
        
        while(peek().tokenType != TokenType::EndOfLine){
            if (peek().tokenType == TokenType::Semicolon) {
                // Empty statement consume semicolon and continue
                consume();
                continue;
            }   

            unique_ptr<ASTNode> expr = parseStatement();
            if(!expr){cerr<<"Error : problem when parsing expression";}

            bool isBlockish =
                dynamic_cast<IfNode*>(expr.get()) ||
                dynamic_cast<WhileNode*>(expr.get()) ||
                dynamic_cast<ForNode*>(expr.get()) ||
                dynamic_cast<FunctionNode*>(expr.get()) ||
                dynamic_cast<EndNode*>(expr.get());
                    
            if (!isBlockish && !match(TokenType::Semicolon)) {
                cerr << "Syntax Error: expected ';' at position " << pos 
                     << " (near token '" << peek().token << "')" << endl;
                return {};
            }
            
            lines.push_back(move(expr));
        }

        return lines;
    }

    void generateCode(ASTNode* node) {
        static int i = 0;

        if (auto* num = dynamic_cast<NumberNode*>(node)) {
            // Literal value: mov rax, immediate
            assemblyCode.push_back("    mov rax, " + to_string(num->value));
        }
        else if (auto* pn = dynamic_cast<PrintNode*>(node)) {
            if(auto* num = dynamic_cast<NumberNode*>(pn->value.get())){
                assemblyCode.push_back("    mov rdi, " + to_string(num->value)); // put number in rdi
                assemblyCode.push_back("    call print_number");
            }
            else if(auto* str = dynamic_cast<StringNode*>(pn->value.get())){             
                string label = "temp" + to_string(i);
                assemblyCode.insert(assemblyCode.begin() + data++, label + "_msg db " + escapeString(str->value) + "");
                assemblyCode.insert(assemblyCode.begin() + data++, label + "_len equ $ - " + label + "_msg");
                assemblyCode.push_back("    mov rsi, " + label + "_msg");
                assemblyCode.push_back("    mov rdx, " + label + "_len");
                assemblyCode.push_back("    call print_string");
                i++;
            }
            else if(auto* vn = dynamic_cast<VariableNameNode*>(pn->value.get())){
                string name = vn->name;
                if(variableType[name] == valueType::Number){
                    int offset = variableOffsets[vn->name];
                    assemblyCode.push_back("    mov rdi, [rbp" + (offset < 0 ? to_string(offset) : "+" + to_string(offset)) + "]");
                    assemblyCode.push_back("    call print_number");
                }
                else if (variableType[name] == valueType::String) {
                    string label = stringStorage[name];
                    assemblyCode.push_back("    mov rsi, " + label + "_msg");
                    assemblyCode.push_back("    mov rdx, " + label + "_len");
                    assemblyCode.push_back("    call print_string");
                }
            }
            else if(auto* idn = dynamic_cast<IncDecNode*>(pn->value.get())){
                generateCode(idn);
                assemblyCode.push_back("    mov rdi, rax");
                assemblyCode.push_back("    call print_number");
            }
            else if(auto* bon = dynamic_cast<BinaryOperatorNode*>(pn->value.get())){
                generateCode(bon);
                assemblyCode.push_back("    mov rdi, rax");
                assemblyCode.push_back("    call print_number");
            }
            else if (auto* un = dynamic_cast<UnaryOperatorNode*>(pn->value.get())){
                generateCode(un);
                assemblyCode.push_back("    mov rdi, rax");
                assemblyCode.push_back("    call print_number");
            }
            else if (auto* fcn = dynamic_cast<FunctionCallNode*>(pn->value.get())){
                generateCode(fcn);
                assemblyCode.push_back("    mov rdi, rax");
                assemblyCode.push_back("    call print_number");
            }
        }
        else if (auto* pn = dynamic_cast<PrintNewLineNode*>(node)) {
            if(auto* num = dynamic_cast<NumberNode*>(pn->value.get())){
                assemblyCode.push_back("    mov rdi, " + to_string(num->value)); // put number in rdi
                assemblyCode.push_back("    call print_number");
            }
            else if(auto* str = dynamic_cast<StringNode*>(pn->value.get())){
                string label = "temp" + to_string(i);
                assemblyCode.insert(assemblyCode.begin() + data++, label + "_msg db " + escapeString(str->value) + "");
                assemblyCode.insert(assemblyCode.begin() + data++, label + "_len equ $ - " + label + "_msg");
                assemblyCode.push_back("    mov rsi, " + label + "_msg");
                assemblyCode.push_back("    mov rdx, " + label + "_len");
                assemblyCode.push_back("    call print_string");
                i++;
            }
            else if(auto* vn = dynamic_cast<VariableNameNode*>(pn->value.get())){
                string name = vn->name;
                if(variableType[name] == valueType::Number){
                    int offset = variableOffsets[vn->name];
                    assemblyCode.push_back("    mov rdi, [rbp" + (offset < 0 ? to_string(offset) : "+" + to_string(offset)) + "]");
                    assemblyCode.push_back("    call print_number");
                }
                else if (variableType[name] == valueType::String) {
                    string label = stringStorage[name];
                    assemblyCode.push_back("    mov rsi, " + label + "_msg");
                    assemblyCode.push_back("    mov rdx, " + label + "_len");
                    assemblyCode.push_back("    call print_string");
                }
            }
            else if(auto* idn = dynamic_cast<IncDecNode*>(pn->value.get())){
                generateCode(idn);
                assemblyCode.push_back("    mov rdi, rax");
                assemblyCode.push_back("    call print_number");
            }
            else if(auto* bon = dynamic_cast<BinaryOperatorNode*>(pn->value.get())){
                generateCode(bon);
                assemblyCode.push_back("    mov rdi, rax");
                assemblyCode.push_back("    call print_number");
            }
            else if (auto* un = dynamic_cast<UnaryOperatorNode*>(pn->value.get())){
                generateCode(un);
                assemblyCode.push_back("    mov rdi, rax");
                assemblyCode.push_back("    call print_number");
            }
            else if (auto* fcn = dynamic_cast<FunctionCallNode*>(pn->value.get())){
                generateCode(fcn);
                assemblyCode.push_back("    mov rdi, rax");
                assemblyCode.push_back("    call print_number");
            }
            assemblyCode.push_back("    ; newline");
            assemblyCode.push_back("    mov rax, 1");
            assemblyCode.push_back("    mov rdi, 1");
            assemblyCode.push_back("    mov rsi, newline");
            assemblyCode.push_back("    mov rdx, 1");
            assemblyCode.push_back("    syscall");
        }
        else if (auto* var = dynamic_cast<VariableNameNode*>(node)) {
            // Load variable at [rbp + offset] into rax
            if (variableOffsets.count(var->name)){
                int offset = variableOffsets[var->name];
                string offstr = (offset < 0 ? to_string(offset) : "+" + to_string(offset));
                assemblyCode.push_back("    mov rax, [rbp" + offstr + "]");
            }else {
                cerr << "undefined variable: " << var->name << endl;
                exit(1);
            }       
        }
        else if (auto* assign = dynamic_cast<AssignmentNode*>(node)) {
            if(auto* str = dynamic_cast<StringNode*>(assign->value.get())){
                string label = "str_" + assign->variableName;
                stringStorage[assign->variableName] = label;
                //variableType[assign->variableName] = valueType::String;
                assemblyCode.insert(assemblyCode.begin() + data++, label + "_msg db " + escapeString(str->value) + "");
                assemblyCode.insert(assemblyCode.begin() + data++, label + "_len equ $ - " + label + "_msg");
                // No assembly needed for storing strings in stack
            }
            else{
                generateCode(assign->value.get());  // Result in rax
                int offset;
                if(variableOffsets.find(assign->variableName) == variableOffsets.end()) {cerr << "variabe used before declaration\n";exit(0);}
                else offset = variableOffsets[assign->variableName];
                //int offset = allocateVariable(assign->variableName);
                assemblyCode.push_back("    mov [rbp" + (offset < 0 ? to_string(offset) : "+" + to_string(offset)) + "], rax");
                //variableType[assign->variableName] = valueType::Number;
            }
        }
        else if(auto* in = dynamic_cast<IfNode*>(node)){
            static int lableCount = 0;
            int id = lableCount++;

            string endLable = "endif_" + to_string(id);
            vector<string> jumpLabels;

            for (size_t i = 0; i < in->conditionalBlock.size(); i++){
                const auto& [condition,block] = in->conditionalBlock[i];
                string nextLabel = "else_if_" + to_string(id) + "_" + to_string(i);
                jumpLabels.push_back(nextLabel);

                generateCode(condition.get()); // result in rax
                assemblyCode.push_back("    cmp rax, 0");
                assemblyCode.push_back("    je " + nextLabel);
                
                for(auto& stmt : block) generateCode(stmt.get());
                assemblyCode.push_back("    jmp " + endLable);
                assemblyCode.push_back(nextLabel + ":");
            }
            if (!in->elseBlock.empty()) for (auto& stmt : in->elseBlock) generateCode(stmt.get());

            assemblyCode.push_back(endLable + ":");
        }
        else if(auto* wn = dynamic_cast<WhileNode*>(node)){
            static int loopId = 0;
            int id = loopId++;

            string startLabel = "while_start_" + to_string(id);
            string endLabel = "while_end_" + to_string(id);

            continueLabel.push_back(startLabel);
            breakLabel.push_back(endLabel);

            assemblyCode.push_back(startLabel +":");
            generateCode(wn->condition.get());
            assemblyCode.push_back("    cmp rax, 0");
            assemblyCode.push_back("    je " + endLabel);
            for(auto& stmt : wn->body) generateCode(stmt.get());
            assemblyCode.push_back("    jmp " + startLabel);
            assemblyCode.push_back(endLabel+":");

            continueLabel.pop_back();
            breakLabel.pop_back();
        }
        else if (auto* fn = dynamic_cast<ForNode*>(node)){
            static int loopId = 0;
            int id = loopId++;

            string startLabel = "for_start_" + to_string(id);
            string endLabel = "for_end_" + to_string(id);

            continueLabel.push_back(startLabel);
            breakLabel.push_back(endLabel);

            if (fn->initialization) generateCode(fn->initialization.get());
            assemblyCode.push_back(startLabel + ":");
            if(fn->condition) {
                generateCode(fn->condition.get());
                assemblyCode.push_back("    cmp rax, 0");
                assemblyCode.push_back("    je " + endLabel);
            }

            for (auto& stmt : fn->body) generateCode(stmt.get());

            if (fn->update) generateCode(fn->update.get());
            assemblyCode.push_back("    jmp " + startLabel);
            assemblyCode.push_back(endLabel+":");

            continueLabel.pop_back();
            breakLabel.pop_back();
        }
        else if (auto* idn = dynamic_cast<IncDecNode*>(node)){
            if(variableOffsets.find(idn->varName) == variableOffsets.end()){
                cerr << "Error: variable '" << idn->varName << "' not declared\n";
                exit(1);
            }
            int offset = variableOffsets[idn->varName];
            string off = "[rbp" + (offset < 0 ? to_string(offset) : "+" + to_string(offset)) + "]";
            switch(idn->type){
                case IncDecType::PreIncrement:
                    assemblyCode.push_back("    inc qword " + off);
                    assemblyCode.push_back("    mov rax, "+ off);
                    break;
                case IncDecType::PostIncrement:
                    assemblyCode.push_back("    mov rax, "+ off);
                    assemblyCode.push_back("    inc qword " + off);
                    break;
                case IncDecType::PreDecrement:
                    assemblyCode.push_back("    dec qword " + off);
                    assemblyCode.push_back("    mov rax, "+ off);
                    break;
                case IncDecType::PostDecrement:
                    assemblyCode.push_back("    mov rax, "+ off);
                    assemblyCode.push_back("    dec qword " + off);
                    break;
            }
        }
        else if (auto* fn = dynamic_cast<FunctionNode*>(node)){
            unordered_map<string, int64_t> oldOffset = variableOffsets;
            variableOffsets.clear();
            size_t safeStart = assemblyCode.size();
            

            if(fn->parameters.size() > 6){
                cerr << "Too many arguments" << endl;
                exit(1);
            }
            vector<string> funCode;
            while (funCode.size() < 5) {
                funCode.push_back(""); // or any placeholder string
            }
            int insertIndex = 5;
            int parmOffset = -8;
            string regs[] = {"rdi","rsi","rdx","rcx","r8","r9"};
            for (int i = 0; i < fn->parameters.size(); i++)
            {
                string line = "    mov [rbp" + to_string(parmOffset) + "], " + regs[i];
                funCode.insert(funCode.begin() + insertIndex, line);
                variableOffsets[fn->parameters[i]] = parmOffset;
                parmOffset -= 8;
                insertIndex++; // shift index because vector grows
            }


            string endLabel = fn->name + "_end";
            currentFunctionEndLabel = endLabel;

            for (auto& stmt : fn->body) generateCode(stmt.get());
            
            int endIndex = assemblyCode.size();
            funCode.insert(funCode.begin(), "global " + fn->name);
            funCode.insert(funCode.begin() + 1, fn->name + ":");
            funCode.insert(funCode.begin() + 2, "    push rbp");
            funCode.insert(funCode.begin() + 3, "    mov rbp, rsp");
            funCode.insert(funCode.begin() + 4, "    sub rsp, " + to_string(fn->parameters.size() * 8));

            //for (int i = 0; i < fn->parameters.size(); i++)
            //{
            //    funCode.push_back("    mov [rbp" + to_string(parmOffset) + "], " + regs[i]);
            //    variableOffsets[fn->parameters[i]] = parmOffset;
            //    parmOffset -= 8;
//
            //}

            funCode.insert(funCode.end(), assemblyCode.begin() + safeStart, assemblyCode.begin() + endIndex); // the code of function into funcode vector 
            
            funCode.push_back(currentFunctionEndLabel + ":");
            if (!isFunRetValue(fn)){           
                funCode.push_back("    mov rax, -1");
            }
                funCode.push_back("    leave");
                funCode.push_back("    ret");
            //funCode.push_back("    jmp " + endLabel);
            
            // idea is that we add the data or body statement of function into funCode with auto startIndex and endIndex and then remove it from assemblyCode you get it right man it was your idea btw :)
            
            // remove's the data or instuction from assembly code
            assemblyCode.erase(assemblyCode.begin() + safeStart , assemblyCode.begin() + endIndex);

            for (const auto& line: funCode){
                cout << line << endl;
            }
            // inserts it to top of the assembly code and increment the index = 'data'
            assemblyCode.insert(assemblyCode.begin() + ++data, funCode.begin(), funCode.end());
            data += funCode.size();
            variableOffsets = oldOffset;

        }
        else if (auto* fcn = dynamic_cast<FunctionCallNode*>(node)){
            string regs[] = {"rdi","rsi","rdx","rcx","r8","r9"};
            if(fcn->arguments.size() > 6){
                cerr << "Too many arguments" << endl;
                exit(1);
            }
            for (int i = 0;i < fcn->arguments.size();i++){
                generateCode(fcn->arguments[i].get());
                assemblyCode.push_back("    mov " + regs[i] + ", rax");
            }

            assemblyCode.push_back("    call " + fcn->name);
        }
        else if (auto* rn = dynamic_cast<ReturnNode*>(node)){
            generateCode(rn->value.get());
            
            if(currentFunctionEndLabel.empty()){
                cerr << "Error: return statement used outsite the function\n";
                exit(1);
            }

            assemblyCode.push_back("    jmp " + currentFunctionEndLabel);
        }
        else if (auto* en = dynamic_cast<ExitNode*>(node)) {
            generateCode(en->value.get()); 
            assemblyCode.push_back("    mov rdi, rax");   // 1st argument to exit
            assemblyCode.push_back("    mov rax, 60");    // syscall number for exit
            assemblyCode.push_back("    syscall");
        }
        else if (auto* en = dynamic_cast<EndNode*>(node)) {
            assemblyCode.push_back("    mov rax, 60");   // 1st argument to exit
            assemblyCode.push_back("    xor rdi,rdi");    // syscall number for exit
            assemblyCode.push_back("    syscall");
        }
        else if (auto* binOp = dynamic_cast<BinaryOperatorNode*>(node)) {
            int andCount = 0;
            int orCount = 0;
            // Evaluate left operand
            generateCode(binOp->left.get());
            assemblyCode.push_back("    push rax");  // Save left value on stack

            // Evaluate right operand
            generateCode(binOp->right.get());
            assemblyCode.push_back("    mov rbx, rax");  // Move right operand to rbx

            assemblyCode.push_back("    pop rax");        // Restore left operand to rax

            string op = binOp->op;
            
            if (op == "+" || op == "-" || op == "*" || op == "/" || op == "%"){
                if(op == "+") assemblyCode.push_back("    add rax, rbx");
                else if(op == "-") assemblyCode.push_back("    sub rax, rbx");
                else if(op == "*") assemblyCode.push_back("    imul rax, rbx");
                else if(op == "/") {
                    assemblyCode.push_back("    cqo");          // Sign extend rax -> rdx:rax
                    assemblyCode.push_back("    idiv rbx");     // Divide rdx:rax by rbx
                }
                else if(op == "%") {
                    assemblyCode.push_back("    cqo");
                    assemblyCode.push_back("    idiv rbx");
                    assemblyCode.push_back("    mov rax, rdx");
                }
            }
            else if (op == "&&"){
                string falseLabel = "and_false_" + to_string(andCount);
                string endLabel = "and_end_" + to_string(andCount);
                andCount++;
                generateCode(binOp->left.get());
                assemblyCode.push_back("    cmp rax, 0");
                assemblyCode.push_back("    je " + falseLabel);

                generateCode(binOp->right.get());
                assemblyCode.push_back("    cmp rax, 0");
                assemblyCode.push_back("    je " + falseLabel);

                assemblyCode.push_back("    mov rax, 1");
                assemblyCode.push_back("    jmp " + endLabel);

                assemblyCode.push_back(falseLabel + ":");
                assemblyCode.push_back("    mov rax, 0");

                assemblyCode.push_back(endLabel + ":");
            }
            else if (op == "||"){
                string trueLabel = "or_false_" + to_string(orCount);
                string endLabel = "or_end_" + to_string(orCount);
                orCount++;
                generateCode(binOp->left.get());
                assemblyCode.push_back("    cmp rax, 0");
                assemblyCode.push_back("    je " + trueLabel);

                generateCode(binOp->right.get());
                assemblyCode.push_back("    cmp rax, 0");
                assemblyCode.push_back("    je " + trueLabel);

                assemblyCode.push_back("    mov rax, 0");
                assemblyCode.push_back("    jmp " + endLabel);

                assemblyCode.push_back(trueLabel + ":");
                assemblyCode.push_back("    mov rax, 1");

                assemblyCode.push_back(endLabel + ":");
            }
            else{
                assemblyCode.push_back("    cmp rax, rbx");
                string setInstr;
                if(op == "==") setInstr = "sete";
                else if (op == "!=") setInstr = "setne";
                else if (op == "<") setInstr = "setl"; 
                else if (op == "<=") setInstr = "setle";
                else if (op == ">") setInstr = "setg";
                else if (op == ">=") setInstr = "setge";
                assemblyCode.push_back("    " + setInstr + " al");
                assemblyCode.push_back("    movzx rax, al");
            }
        }
        else if (auto* un = dynamic_cast<UnaryOperatorNode*>(node)){
            generateCode(un->operand.get());
            if (un->op == "-") assemblyCode.push_back("    neg rax");
            else if (un->op == "!") {
                assemblyCode.push_back("    cmp rax, 0");
                assemblyCode.push_back("    mov rax, 0");
                assemblyCode.push_back("    sete al");
            }
            else {
                cerr << "Unsupported unary operator: " << un->op <<endl;
                exit(1);
            }
        }
        else if (dynamic_cast<BreakNode*>(node)){
            if(breakLabel.empty()){
                cerr << "Error: 'break' used outside of loop"<<endl;
                exit(1);
            }
            assemblyCode.push_back("    jmp " + breakLabel.back());
        }
        else if (dynamic_cast<ContinueNode*>(node)){
            if(continueLabel.empty()){
                cerr << "Error: 'continue' used outside of loop"<<endl;
                exit(1);
            }
            assemblyCode.push_back("    jmp " + continueLabel.back());
        }
        else if (auto* input = dynamic_cast<InputNode*>(node)){
            assemblyCode.push_back("    ; input()");
            assemblyCode.push_back("    mov rax, 0");
            assemblyCode.push_back("    mov rdi, 0");
            assemblyCode.push_back("    mov rsi, buffer");
            assemblyCode.push_back("    mov rdx, 32");
            assemblyCode.push_back("    syscall");
            assemblyCode.push_back("    mov rsi, buffer");
            assemblyCode.push_back("    call atoi");
        }
    }

    void addFunctions(){
        assemblyCode.push_back("    mov rax, 60");   // 1st argument to exit
        assemblyCode.push_back("    xor rdi,rdi");    // syscall number for exit
        assemblyCode.push_back("    syscall");
        assemblyCode.push_back("; ----------------------");
        assemblyCode.push_back("; void print_number(int64_t value)");
        assemblyCode.push_back("; number is passed in rdi");                
        assemblyCode.push_back("print_number:");
        assemblyCode.push_back("    mov rax, rdi");
        assemblyCode.push_back("    mov rbx, 10");
        assemblyCode.push_back("    mov rsi, buffer + 32");  // start from end
        assemblyCode.push_back("    mov rcx, 0");            // char count
        assemblyCode.push_back("    mov rdx, 0");            // clear remainder
        assemblyCode.push_back("    cmp rdi, 0");
        assemblyCode.push_back("    jl .negative");          // if rdi < 0
        assemblyCode.push_back(".convert:");
        assemblyCode.push_back("    xor rdx, rdx");
        assemblyCode.push_back("    div rbx");
        assemblyCode.push_back("    add dl, '0'");
        assemblyCode.push_back("    dec rsi");
        assemblyCode.push_back("    mov [rsi], dl");
        assemblyCode.push_back("    inc rcx");
        assemblyCode.push_back("    test rax, rax");
        assemblyCode.push_back("    jnz .convert");
        assemblyCode.push_back("    jmp .print");
        assemblyCode.push_back(".negative:");
        assemblyCode.push_back("    neg rax");
        assemblyCode.push_back("    xor rdx, rdx");
        assemblyCode.push_back("    .negconvert:");
        assemblyCode.push_back("    div rbx");
        assemblyCode.push_back("    add dl, '0'");
        assemblyCode.push_back("    dec rsi");
        assemblyCode.push_back("    mov [rsi], dl");
        assemblyCode.push_back("    inc rcx");
        assemblyCode.push_back("    test rax, rax");
        assemblyCode.push_back("    jnz .negconvert");
        assemblyCode.push_back("    dec rsi");
        assemblyCode.push_back("    mov byte [rsi], '-'");
        assemblyCode.push_back("    inc rcx");
        assemblyCode.push_back(".print:");
        assemblyCode.push_back("    mov rax, 1");
        assemblyCode.push_back("    mov rdi, 1");
        assemblyCode.push_back("    mov rdx, rcx");
        assemblyCode.push_back("    syscall");
        assemblyCode.push_back("    ret");

        assemblyCode.push_back("; ----------------------");
        assemblyCode.push_back("; atoi");
        assemblyCode.push_back("; convert buffer (rsi) to integer in rax");
        assemblyCode.push_back("atoi:");
        assemblyCode.push_back("    xor rax, rax");
        assemblyCode.push_back("    xor rbx, rbx");
        assemblyCode.push_back(".loop:");
        assemblyCode.push_back("    mov bl, byte [rsi]");
        assemblyCode.push_back("    cmp bl, 10");
        assemblyCode.push_back("    je .done");
        assemblyCode.push_back("    cmp bl, '0'");
        assemblyCode.push_back("    jl .done");
        assemblyCode.push_back("    cmp bl, '9'");
        assemblyCode.push_back("    jg .done");
        assemblyCode.push_back("    sub bl, '0'");
        assemblyCode.push_back("    imul rax, rax, 10");
        assemblyCode.push_back("    add rax, rbx");
        assemblyCode.push_back("    inc rsi");
        assemblyCode.push_back("    jmp .loop");
        assemblyCode.push_back(".done:");
        assemblyCode.push_back("    ret");

        assemblyCode.push_back("; ----------------------");
        assemblyCode.push_back("; void print_string(char* str, uint64_t len)");
        assemblyCode.push_back("; rsi = pointer, rdx = length");
        assemblyCode.push_back("print_string:");
        assemblyCode.push_back("    push rsi");
        assemblyCode.push_back("    push rdx");
        assemblyCode.push_back("    mov rax, 1");
        assemblyCode.push_back("    mov rdi, 1");
        assemblyCode.push_back("    syscall");
        assemblyCode.push_back("    pop rdx");
        assemblyCode.push_back("    pop rsi");
        assemblyCode.push_back("    ret");

    }
};

// ========== File Management Functions ============

string readFile(string filename){
    ifstream file(filename);
    if(!file){
        cerr << "There was problem in reading files";
        return "";
    }

    stringstream out;
    out << file.rdbuf();
    return out.str();
}

void saveFile(vector<string> lines){
    ofstream outFile("build/hello.asm",ios::trunc);
    if (!outFile) {
        std::cerr << "Failed to open file.\n";
    }
    for(const string& line : lines){
        outFile << line << endl;
    }
    outFile.close();
    cout<<"file saved!\n";
}   

void runCommands(){
    system("nasm -f elf64 build/hello.asm -o build/hello.o");
    system("ld build/hello.o -o build/hello");
    system("./build/hello");
}

// ================ Main Function ==================

int main(int argc,char* argv[]){
    
    if(argc < 2 ){
        cerr<< "Error : CLI Argument" << endl;
        return 1;
    }

    string input = readFile(argv[1]);
    
    // string input = "2123 + 3134 * 314";
    Parser parser(input);
    vector<unique_ptr<ASTNode>> lines = parser.parse();
    
    for(const auto& ast : lines){
        
        cout<<"ast : ";
        if (!ast) {
            cout << "AST is null (parse failed)" << endl;
        } else {
            ast->print();
        }
        cout<<endl;
        parser.generateCode(ast.get());
    }
    parser.addFunctions();
    saveFile(parser.assemblyCode);
    runCommands();
    return 0;
}