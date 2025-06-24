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
enum TokenType{

    // Variable Token

    Number_Token,
    String_Token,
    
    // Keywords Token

    Let_Token,
    PrintNewLine_Token,
    Print_Token,
    If_Token,
    ElseIf_Token,
    Else_Token,
    Exit_Token,
    End_Token,

    // General Token

    Semicolon_Token,
    EndOfLine_Token,
    Identifier_Token,
    DoubleQuotes_Token,

    // Arithmatic Tokens

    Plus_Token,
    Minus_Token,
    Star_Token,
    Slash_Token,

    // Assignment Tokens

    Equal_Token,

    // Compare Token

    EqualEqual_Token,
    NotEqual_Token,
    Less_Token,
    LessEqual_Token,
    Greater_Token,
    GreaterEqual_Token,

    // Brackets Tokens

    Open_Brace_Token,
    Close_Brace_Token,
    Open_Parentheses_Token,
    Close_Parentheses_Token,

    // Unknown Tokens

    Unknown_Token,
};

/// @brief This function is like util for debug that converts enum if TokenType into String so we can easy print it
/// @param type TokenType
/// @return TokenType into String
string TokenTypeToString(TokenType type) {
    switch (type) {
        case Number_Token: return "Number_Token";
        case String_Token: return "String_Token";
        case Semicolon_Token: return "Semicolon_Token";
        case EndOfLine_Token: return "EndOfLine_Token";
        case EqualEqual_Token : return "EqualEqual_Token";
        case NotEqual_Token : return "NotEqual_Token";
        case Less_Token : return "Less_Token";
        case LessEqual_Token : return "LessEqual_Token";
        case Greater_Token : return "Greater_Token";        
        case GreaterEqual_Token : return "GreaterEqual_Token";
        case DoubleQuotes_Token: return "DoubleQuotes_Token";
        case Plus_Token: return "Plus_Token";
        case Minus_Token: return "Minus_Token";
        case Star_Token: return "Star_Token";
        case Slash_Token: return "Slash_Token";
        case Equal_Token: return "Equal_Token";
        case Exit_Token: return "Exit_Token";
        case End_Token: return "End_Token";
        case Print_Token: return "Print_Token";
        case PrintNewLine_Token: return "PrintNewLine_Token";
        case Identifier_Token: return "Identifier_Token";
        case Let_Token: return "Let_Token";
        case If_Token: return "If_Token";
        case ElseIf_Token: return "ElseIf_Token";
        case Else_Token: return "Else_Token";
        case Open_Brace_Token: return "Open_Brace_Token";
        case Close_Brace_Token: return "Close_Brace_Token";
        case Open_Parentheses_Token: return "Open_Parentheses_Token";
        case Close_Parentheses_Token: return "Close_Parentheses_Token";
        case Unknown_Token: return "Unknown_Token";
        default: return "Error : Unknown_TokenType";
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
        {'+', Plus_Token},
        {'-', Minus_Token},
        {'*', Star_Token},
        {'/', Slash_Token},
        {'=', Equal_Token},
        {'{', Open_Brace_Token},
        {'}', Close_Brace_Token},
        {'(', Open_Parentheses_Token},
        {')', Close_Parentheses_Token},
        {';', Semicolon_Token},
        {'<', Less_Token},  
        {'>', Greater_Token}            
    };

    unordered_map<string,TokenType> keywordToken = {
        {"laile",Let_Token},
        {"println",PrintNewLine_Token},
        {"print",Print_Token},
        {"if",If_Token},
        {"elif",ElseIf_Token},
        {"else",Else_Token},        
        {"exit",Exit_Token},
        {"end",End_Token}
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
            if(isdigit(ch)){
                int _pos = pos;
                int value = parseInteger();
                Tokens.push_back({_pos,Number_Token,to_string(value),value});
            }

            else if (peek() == '=' && peek(1) == '=') {
                Tokens.push_back({pos,EqualEqual_Token,"==",0});
                consume(2);
            }


            else if (peek() == '!' && peek(1) == '=') {
                Tokens.push_back({pos,NotEqual_Token,"!=",0});
                consume(2);
            }


            else if (peek() == '<' && peek(1) == '=') {
                Tokens.push_back({pos,LessEqual_Token,"<=",0});
                consume(2);
            }


            else if (peek() == '>' && peek(1) == '=') {
                Tokens.push_back({pos,GreaterEqual_Token,">=",0});
                consume(2);
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
            
                Tokens.push_back({pos,String_Token,strValue,0});
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
                    Tokens.push_back({_pos,Identifier_Token,id,0});
                }
            }

            else if(isspace(ch)){
                consume();            
            }

            else{
                Tokens.push_back({pos,Unknown_Token,string(1,ch),0});
                cerr<<"Error : Unexpected token" << ch << " position " << pos <<endl;
                consume();
            }
        }
        Tokens.push_back({pos,EndOfLine_Token,"EOfL",0});
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
    EndNode(){};
    void print() const override {
        cout<<"-------- code ended ----------";
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



// ================ Parser ==================

/// @brief This is For converting or parsing tokens into ast (Abtract Syntax Tree)
class Parser{
private:

    unique_ptr<Lexer> lexer;
    vector<Token> Tokens;
    enum class valueType{ Number, String};
    unordered_map<string, valueType> variableType;
    unordered_map<string, string> stringStorage;
    unordered_map<string, int64_t> variableOffsets;
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
            return {pos + offset, EndOfLine_Token, "", 0};
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
        if(peek().tokenType == Exit_Token){
            consume(); // exit token
            if(!match(Open_Parentheses_Token)){
                cerr << "Syntax Error: expected '(' after 'exit'" << endl;
                return nullptr;
            }
            auto expr = parseExpression();

            if(!match(Close_Parentheses_Token)){
                cerr << "Syntax Error: expected ')' after exit expression" << endl;
                return nullptr;
            }

            return make_unique<ExitNode>(unique_ptr<ASTNode>(expr.release()));
        }
        else if(peek().tokenType == PrintNewLine_Token){
            consume(); // print token
            unique_ptr<ASTNode> expr;
            if(!match(Open_Parentheses_Token)){
                cerr << "Syntax Error: expected '(' after 'print'" << endl;
                return nullptr;
            }
            if(peek().tokenType == String_Token){
                string str = consume().token;
                expr = make_unique<StringNode>(str);
            }else{
                expr = parseExpression();
            }

            if(!match(Close_Parentheses_Token)){
                cerr << "Syntax Error: expected ')' after print expression" << endl;
                return nullptr;
            }
            return make_unique<PrintNewLineNode>(move(expr));
        }
        else if(peek().tokenType == Print_Token){
            consume(); // print token
            unique_ptr<ASTNode> expr;
            if(!match(Open_Parentheses_Token)){
                cerr << "Syntax Error: expected '(' after 'print'" << endl;
                return nullptr;
            }

            if(peek().tokenType == String_Token){
                
                string str = consume().token;
                expr = make_unique<StringNode>(str);
            }else{
                expr = parseExpression();
            }

            if(!match(Close_Parentheses_Token)){
                cerr << "Syntax Error: expected ')' after print expression" << endl;
                return nullptr;
            }
            return make_unique<PrintNode>(move(expr));
        }
        else if(peek().tokenType == Let_Token && peek(1).tokenType == Identifier_Token && peek(2).tokenType == Equal_Token){
            consume(); // let token
            string variableName = consume().token;
            consume(); // =
            unique_ptr<ASTNode> expr;
            if(peek().tokenType == String_Token){
                string str = consume().token;
                expr = make_unique<StringNode>(str);
            }else{
                expr = parseExpression();
            }
            return make_unique<AssignmentNode>(variableName,move(expr));
        }
        else if (peek().tokenType == If_Token){
            return parseIfStatement();
        }
        else if(peek().tokenType == End_Token){
            consume();
            return make_unique<EndNode>();
        }
        return parseExpression();
    }

    /// @brief for making + and - token into binary expression
    /// @return a full binary expression with operator + or -
    unique_ptr<ASTNode> parseExpression(){
        auto node = parseTerm();
        while(true){

            TokenType tt = peek().tokenType;

            if (tt == EqualEqual_Token || tt == NotEqual_Token || tt == Less_Token || tt == LessEqual_Token ||
                tt == Greater_Token || tt == GreaterEqual_Token || tt == Plus_Token || tt == Minus_Token){

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
        while(peek().tokenType == Star_Token || peek().tokenType == Slash_Token){
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
        if(match(Number_Token)){
            return make_unique<NumberNode>(Tokens[pos - 1].value);
        }
        else if (match(Identifier_Token)) {
            return make_unique<VariableNameNode>(Tokens[pos - 1].token);
        }
        else if(match(Open_Parentheses_Token)){
            auto node = parseExpression();
            if(!match(Close_Parentheses_Token)){
                cerr<<"Error : expected ')' at position "<< pos << endl;
            }
            return node;
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
            while(!match(Close_Brace_Token)){
                auto stmt = parseStatement();
                if (!stmt || !match(Semicolon_Token)){
                    cerr << "Expected ';' inside if block" << endl;
                    return vector<unique_ptr<ASTNode>>();
                }
                block.push_back(move(stmt));
            }
            return block;
        };

        auto parseConditionAndBlock = [&]() -> pair<unique_ptr<ASTNode>,vector<unique_ptr<ASTNode>>> {
            if(!match(Open_Parentheses_Token)){
                cerr<<"Expected '(' after 'if'" << endl;
                return {};
            }

            auto condi = parseExpression();

            if(!match(Close_Parentheses_Token)){
                cerr<<"Expected ')' after 'condition'" << endl;
                return {};
            }

            if(!match(Open_Brace_Token)){
                cerr<<"Expected '{' to start if block" << endl;
                return {};
            }


            auto block = parseBlock();

            return {move(condi),move(block)};    
        };
        

        consume(); // if token
        
        conditionalBlock.push_back(parseConditionAndBlock());

        while(match(ElseIf_Token)) {
            conditionalBlock.push_back(parseConditionAndBlock());
        }
        
        if (match(Else_Token)){
            if(!match(Open_Brace_Token)){
                cerr << "Expected '{' after 'else'" << endl;
                return nullptr;
            }
            elseBlock = parseBlock();
        }

        return make_unique<IfNode>(move(conditionalBlock),move(elseBlock));
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
            for (auto& token : Tokens) {
                token.printTokens();
            }
            cout << "==============\n\n\n";
        }
    }
    /// @brief for actual parsing tokens into expression
    /// @return whole ast
    vector<unique_ptr<ASTNode>> parse(){

        vector<unique_ptr<ASTNode>> lines;
        
        while(peek().tokenType != EndOfLine_Token){
            if (peek().tokenType == Semicolon_Token) {
                // Empty statement consume semicolon and continue
                consume();
                continue;
            }   

            unique_ptr<ASTNode> expr = parseStatement();
            if(!expr){cerr<<"Error : problem when parsing expression";}

            if(!match(Semicolon_Token)){
                cerr<< "Syntax Error: expected ';' at position " << pos 
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
        if (auto* pn = dynamic_cast<PrintNode*>(node)) {
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

        }
        if (auto* pn = dynamic_cast<PrintNewLineNode*>(node)) {
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
            assemblyCode.push_back("    ; newline");
            assemblyCode.push_back("    mov rax, 1");
            assemblyCode.push_back("    mov rdi, 1");
            assemblyCode.push_back("    mov rsi, newline");
            assemblyCode.push_back("    mov rdx, 1");
            assemblyCode.push_back("    syscall");
        }
        else if (auto* var = dynamic_cast<VariableNameNode*>(node)) {
            // Load variable at [rbp + offset] into rax
            int offset = variableOffsets[var->name];
            assemblyCode.push_back("    mov rax, [rbp" + (offset < 0 ? to_string(offset) : "+" + to_string(offset)) + "]");
        }
        else if (auto* assign = dynamic_cast<AssignmentNode*>(node)) {
            if(auto* str = dynamic_cast<StringNode*>(assign->value.get())){
                string label = "str_" + assign->variableName;
                stringStorage[assign->variableName] = label;
                variableType[assign->variableName] = valueType::String;
                assemblyCode.insert(assemblyCode.begin() + data++, label + "_msg db " + escapeString(str->value) + "");
                assemblyCode.insert(assemblyCode.begin() + data++, label + "_len equ $ - " + label + "_msg");
                // No assembly needed for storing strings in stack
            }
            else{
                generateCode(assign->value.get());  // Result in rax
                int offset = allocateVariable(assign->variableName);
                assemblyCode.push_back("    mov [rbp" + (offset < 0 ? to_string(offset) : "+" + to_string(offset)) + "], rax");
                variableType[assign->variableName] = valueType::Number;
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
            // Evaluate left operand
            generateCode(binOp->left.get());
            assemblyCode.push_back("    push rax");  // Save left value on stack

            // Evaluate right operand
            generateCode(binOp->right.get());
            assemblyCode.push_back("    mov rbx, rax");  // Move right operand to rbx

            assemblyCode.push_back("    pop rax");        // Restore left operand to rax

            string op = binOp->op;
            
            if (op == "+" || op == "-" || op == "*" || op == "/"){
                if(op == "+") assemblyCode.push_back("    add rax, rbx");
                else if(op == "-") assemblyCode.push_back("    sub rax, rbx");
                else if(op == "*") assemblyCode.push_back("    imul rax, rbx");
                else if(op == "/") {
                    assemblyCode.push_back("    cqo");          // Sign extend rax -> rdx:rax
                    assemblyCode.push_back("    idiv rbx");     // Divide rdx:rax by rbx
                }
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
    }

    void addFunctions(){
        assemblyCode.push_back("    mov rax, 60");   // 1st argument to exit
        assemblyCode.push_back("    xor rdi,rdi");    // syscall number for exit
        assemblyCode.push_back("    syscall");
        assemblyCode.push_back("; ----------------------");
        assemblyCode.push_back("; void print_number(uint64_t value)");
        assemblyCode.push_back("; number is passed in rdi");
        assemblyCode.push_back("print_number:");
        assemblyCode.push_back("    mov rax, rdi");
        assemblyCode.push_back("    lea rsi, [buffer + 20]");
        assemblyCode.push_back("    mov rcx, 0");

        assemblyCode.push_back(".convert_loop:");
        assemblyCode.push_back("    xor rdx, rdx");
        assemblyCode.push_back("    mov rbx, 10");
        assemblyCode.push_back("    div rbx");
        assemblyCode.push_back("    add dl, '0'");
        assemblyCode.push_back("    dec rsi");
        assemblyCode.push_back("    mov [rsi], dl");
        assemblyCode.push_back("    inc rcx");
        assemblyCode.push_back("    test rax, rax");
        assemblyCode.push_back("    jnz .convert_loop");

        assemblyCode.push_back("    ; write result");
        assemblyCode.push_back("    mov rax, 1");
        assemblyCode.push_back("    mov rdi, 1");
        assemblyCode.push_back("    mov rdx, rcx");
        assemblyCode.push_back("    syscall");
        assemblyCode.push_back("    ret");

        assemblyCode.push_back("; ----------------------");
        assemblyCode.push_back("; void print_string(char* str, uint64_t len)");
        assemblyCode.push_back("; rsi = pointer,rdx = length");
        assemblyCode.push_back("print_string:");
        assemblyCode.push_back("    mov rax, 1");
        assemblyCode.push_back("    mov rdi, 1");
        assemblyCode.push_back("    syscall");
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