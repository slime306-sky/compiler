#include <iostream>
#include <string>

using namespace std;

enum TokenType{
    NumberToken,
    WhitespaceToken,
    PlusToken,
    MinusToken,
    SlashToken,
    StarToken,
    OpenParenthesesToken,
    CloseParenthesesToken,
    SemicolonToken,
    EndOfLineToken,
    BadToken
};

const char* TokenTypeToString(TokenType type) {
    switch (type) {
        case NumberToken: return "NumberToken";
        case WhitespaceToken: return "WhitespaceToken";
        case PlusToken: return "PlusToken";
        case MinusToken: return "MinusToken";
        case SlashToken: return "SlashToken";
        case StarToken: return "StarToken";
        case OpenParenthesesToken: return "OpenParenthesesToken";
        case CloseParenthesesToken: return "CloseParenthesesToken";
        case SemicolonToken: return "SemicolonToken";
        case EndOfLineToken: return "EndOfLineToken";
        case BadToken: return "BadToken";
        default: return "UnknownToken";
    }
}


class syntaxToken{
public:
    TokenType Type;
    int Postion;
    string Text;
    string Value;
    
    syntaxToken(TokenType type,int postion,string text,string value){
        Type = type;
        Postion = postion;
        Text = text;
        Value = value;
    }

};

class Laxer{
public:
    string _text;
    int _position = 0;

    Laxer(string text){
        _text = text;
    }

    char current(){
        if(_position >= _text.length()){
            return '\0';
        }
        return _text[_position];
    }

    void next(){
        _position++;
    }

    syntaxToken nextToken(){
        // numbers
        // + - / * ( ) ;
        // whitespace

        if(_position >= _text.length()){
            return syntaxToken(TokenType::EndOfLineToken, _position, "\0", "");
        }


        if(isdigit(current())){
            int start = _position;
            while(isdigit(current()))
                next();
            
            int length = _position - start;
            string text =  _text.substr(start,length);
            string value = text; //  not needed but im stupid
            return syntaxToken(TokenType::NumberToken, start, text, value);
        }

        if(isspace(current())){
            int start = _position;
            while(isspace(current()))
                next();
            
            int length = _position - start;
            string text =  _text.substr(start,length);
            return syntaxToken(TokenType::WhitespaceToken, start, text, "");
        }

        if(current() == '+')
            return syntaxToken(TokenType::PlusToken, _position++, "+", "");
        else if(current() == '-')
            return syntaxToken(TokenType::MinusToken, _position++, "-", "");
        else if(current() == '*')
            return syntaxToken(TokenType::StarToken, _position++, "*", "");
        else if(current() == '/')
            return syntaxToken(TokenType::SlashToken, _position++, "/", "");
        else if(current() == '(')
            return syntaxToken(TokenType::OpenParenthesesToken, _position++, "(", "");
        else if(current() == ')')
            return syntaxToken(TokenType::CloseParenthesesToken, _position++, ")", "");
        else if(current() == ';')
            return syntaxToken(TokenType::SemicolonToken, _position++, ";", "");
        
        return syntaxToken(TokenType::BadToken, _position++, _text.substr(_position - 1, 1), "");

    }
};


int main(){

    string input;
    getline(cin,input);
    Laxer laxer(input);
    while (true)
    {
        syntaxToken token =  laxer.nextToken();
        if(token.Type == TokenType::EndOfLineToken){
            break;
        }
        
        cout << TokenTypeToString(token.Type) << " " << token.Text;
        if(!token.Value.empty())
            cout << " " << token.Value;
        cout<<endl;
    }
    
    return 0;
}
