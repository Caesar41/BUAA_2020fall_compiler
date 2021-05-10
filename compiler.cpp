#include<iostream>
#include<string.h>
#include<stdlib.h>
#include<ctype.h>
#include<map>
#include<fstream>
#include<vector>
#include<string>
#include<stack>
#include<regex>

using namespace std;
int off_i = 8;
string funNameNow = "NULL";
int nowA = 4;   // 多的存换行
int offset = 0x10010000;
int nowFun = off_i; // 函数地址 0-t0 4-t1 8-t2 12-t3 24-ra
int dimension = 1;
int mainA = 0;

int getAddr(int s) {
    if (dimension == 1) {
        int a = nowA;
        nowA += 4 * s;
        return a;
    } else if (dimension == 2) {
        int a = nowFun;
        nowFun += 4 * s;
        return a;
    }
    return -1;
}


typedef enum type {
    ERROR_T,
    VAR,
    CONST,
    FUNC,
    ARRAY,
    TEMP,
    NUL,
    STR,
    INDEX,
    CONST_TEM
} type;

string type_out[] = {
        "NULL", "VAR", "CONST", "FUNC", "ARRAY", "TEMP", "NULL", "STRING", "INDEX", "CONST_TEM"
};

typedef enum data_type {
    ERROR_DT,
    INT,
    CHAR,
    VOID
} dtype;

typedef enum instruct {
    NON,
    PLU,
    MI,
    MU,
    DI,
    PSTR,
    PN,
    PCHAR,
    PINT,
    SCHAR,
    SINT,
    TO,
    J,
    BEQ,
    BNE,
    BLTZ,
    BLEZ,
    IND,
    AR,
    AWD,
    AW,
    FUNIN,
    FUNOUT,
    RET,
    CALL,
    BGTZ,
    BGEZ,
    ERROR_INS,
    PARATO,
    CEND,
    GETR,
    MSP,
    TOMAIN
} instruct;

string instructC[] = {
        "DELETED",
        "PLU", "MI", "MU", "DI", "PSTR", "PN", "PCHAR", "PINT", "SCHAR", "SINT", "TO",
        "J",
        "BEQ",
        "BNE",
        "BLTZ",
        "BLEZ",
        "IND",
        "AR",
        "AWD",
        "AW",
        "FUNIN",
        "FUNOUT",
        "RET",
        "CALL", "BGTZ", "BGEZ", "ERROR_INS", "PARATO", "CEND", "GETR", "MSP", "TOMAIN"
};

typedef struct symbol {
    int n;  // 行号
    type t;
    int temp;   // temp号
    dtype dt;
    string s;
    int d;
    int v1; //记录函数的参数数量
    int value;
    int d1;
    int d2; //维的大小
    int d2_t;
    int ww;
    vector<dtype> para;
    int addr;   //同时记载函数里面总共空间大小
} symbol;

int symIn(symbol &s);

typedef struct middle {
    instruct ins;
    symbol result;
    symbol s1;
    symbol s2;
    symbol s3;
    int d;
    string fun;
} middle;

int hasR = 0;
//int wrongG = 0;//return 用，每次函数清空，由 return 报，是 1 就别重复报了
dtype funcT; //return 用，存当前函数类型
int nNow = 1;
int nline_1;
int nline;
int debug = 1;

void error(char c);

void error(char c, symbol s);

int temN = 0;       // 临时变量的数量
int temIn = 0;  // 标签数量
vector<middle> mCode;
vector<string> strs;
int temS = 0;   // 字符串的数量
symbol one = {.t = CONST, .s = "ONE", .value = 1};
symbol zero = {.t = CONST, .s = "ZERO", .value = 0};
symbol nu = {.t = NUL, .s = "NULL"};
symbol return_refer;

void addCode(instruct i, symbol r, symbol s1, symbol s2) {
    middle m = {.ins = i, .result = r, .s1 = s1, .s2 = s2, .fun = funNameNow};
    mCode.push_back(m);
}

int addCode(middle m) {
    //middle m = {.ins = i, .result = r, .s1 = s1, .s2 = s2};
    m.fun = funNameNow;
    mCode.push_back(m);
    return mCode.size() - 1;
}

int addStr(string s) {
    for (int i = 0; i < strs.size(); i++) {
        if (s == strs[i]) {
            return i;
        }
    }
    strs.push_back(s);
    return strs.size() - 1;
}

typedef enum word {
    IDENFR,
    INTCON,
    CHARCON,
    STRCON,
    CONSTTK,
    INTTK,
    CHARTK,
    VOIDTK,
    MAINTK,
    IFTK,
    ELSETK,
    SWITCHTK,
    CASETK,
    DEFAULTTK,
    WHILETK,
    FORTK,
    SCANFTK,
    PRINTFTK,
    RETURNTK,
    PLUS,
    MINU,
    MULT,
    DIV,
    LSS,
    LEQ,
    GRE,
    GEQ,
    EQL,
    NEQ,
    COLON,
    ASSIGN,
    SEMICN,
    COMMA,
    LPARENT,
    RPARENT,
    LBRACK,
    RBRACK,
    LBRACE,
    RBRACE
} word;

string wordS[] = {
        "IDENFR",
        "INTCON",
        "CHARCON",
        "STRCON",
        "CONSTTK",
        "INTTK",
        "CHARTK",
        "VOIDTK",
        "MAINTK",
        "IFTK",
        "ELSETK",
        "SWITCHTK",
        "CASETK",
        "DEFAULTTK",
        "WHILETK",
        "FORTK",
        "SCANFTK",
        "PRINTFTK",
        "RETURNTK",
        "PLUS",
        "MINU",
        "MULT",
        "DIV",
        "LSS",
        "LEQ",
        "GRE",
        "GEQ",
        "EQL",
        "NEQ",
        "COLON",
        "ASSIGN",
        "SEMICN",
        "COMMA",
        "LPARENT",
        "RPARENT",
        "LBRACK",
        "RBRACK",
        "LBRACE",
        "RBRACE"
};

int isConst(symbol s) {
    return s.t == CONST || s.t == CONST_TEM;
}

int isConst(symbol s1, symbol s2) {
    return isConst(s1) && isConst(s2);
}

int getConst(symbol s) {
    return s.value;
}

int binary2(int n) {
    int T = 0, i = 1;
    int fushu = n < 0;
    n = n < 0 ? -n : n;
    if (n == 0) return 0;
    for (; n > i; T++) {
        i <<= 1;
    }
    if (n == i) {
        return fushu == 0 ? T : -T;
    }
    return 100;
}

int isType(word w) {
    return w == INTTK || w == CHARTK;
}

int addrOf(symbol s) {
    return s.addr + offset;
}

symbol newTemp() {
    symbol s = {.t = TEMP, .temp = ++temN};
    s.d = dimension;
    char tn[30], ttn[30];
    int i = temN, j = 0;
    for (; i > 0; i /= 10, j++) {
        tn[j] = i % 10 + '0';
    }
    for (j--, i = 1, ttn[0] = 'T'; j >= 0; j--, i++) {
        ttn[i] = tn[j];
    }
    ttn[i] = 0;
    s.s = ttn;
    symIn(s);
    //cout << "tem: " << s.s << ", " << s.n << " addr: " << addrOf(s) << endl;
    return s;
}

symbol newIndex() {
    symbol s = {.t = INDEX, .temp = ++temIn};
    s.d = dimension;
    char tn[30], ttn[30];
    int i = temIn, j = 0;
    for (; i > 0; i /= 10, j++) {
        tn[j] = i % 10 + '0';
    }
    for (j--, i = 1, ttn[0] = 'J'; j >= 0; j--, i++) {
        ttn[i] = tn[j];
    }
    ttn[i] = 0;
    s.s = ttn;
    //symIn(s);
    //cout << "tem: " << s.s << ", " << s.n << " addr: " << addrOf(s) << endl;
    return s;
}

typedef struct Content {
    word type;
    char name[5000];
    int n;
    int sn; // 给字符串计数的，以及字符的值
} content, *conLink;

content con[10000];
int top = 0;
int getT = 0;
word sym;

ofstream out;
ofstream outT;
ofstream outM;
ofstream outL;
//ifstream in;

void wrong() {

}

void symput(char s[], word w) {
    strcpy(con[top].name, s);
    con[top].type = w;
    con[top].n = nNow;
    top++;
}

void symput(char s[], word w, int sn) {
    strcpy(con[top].name, s);
    con[top].type = w;
    con[top].sn = sn;
    con[top].n = nNow;
    top++;
}

string sym_name;

stack<symbol> para_stack;
vector<symbol> symList_1;
vector<symbol> symList_2;

vector<symbol> constTem;

void addTem(symbol s) {
    for (int i = 0; i < constTem.size(); i++) {
        if (s.s == constTem[i].s) {
            constTem[i].t = s.t;
            constTem[i].value = s.value;
            return;
        }
    }
    constTem.push_back(s);
}

void updateTem(symbol &s) {
    //cout << "updata!!" << s.s << " now Type: " << s.t << endl;
    if (s.t != TEMP) {
        return;
    }
    for (int i = 0; i < constTem.size(); i++) {
        if (s.s == constTem[i].s) {
            s.t = constTem[i].t;
            s.value = constTem[i].value;
            //cout << "updata conm!!" << s.s << " now Type: " << s.t << endl;
            return;
        }
    }
}

void symget() {
    if (getT > top) {
        printf("\n------ERROR!!!!------\n");
    }
    if (debug == 1) {
        outT << wordS[con[getT - 1].type] << " " << con[getT - 1].name << endl;
    }
    sym = con[getT].type;
    sym_name = con[getT].name;
    nline_1 = nline;
    nline = con[getT].n;
    getT++;
}

int check(word w) {
    int er = 1;
    if (sym != w) {
        if (w == SEMICN) {

            error('k');

        } else if (w == RPARENT) {
            error('l');
        } else if (w == RBRACK) {
            error('m');
        }
        er = 0;
    } else {
        symget();
    }
    return er;
}

int symIn(symbol &s) {
    int size_addr = s.t != ARRAY ? 1 : s.d1 * s.d2;
    s.addr = getAddr(size_addr);
    int re = 0;
    int i;
    if (dimension == 1 && s.t != TEMP) {
        for (i = 0; i < symList_1.size(); i++) {
            if (symList_1[i].s == s.s) {
                re = 1;
            }
        }
    } else if (dimension == 2 && s.t != TEMP) {
        for (i = 0; i < symList_2.size(); i++) {
            if (symList_2[i].s == s.s) {
                re = 1;
            }
        }
    }
    if (re == 1) {
        error('b', s);
    }
    if (dimension == 1) {
        if (debug == 1) {
            cout << "d = 1, " << s.s << ", " << s.n << " addr: " << addrOf(s) << endl;
        }
        s.d = 1;
        symList_1.push_back(s);
        return symList_1.size() - 1;
    } else if (dimension == 2) {
        if (debug == 1) {
            cout << "d = 2, " << s.s << ", " << s.n << " addr: " << addrOf(s) << endl;
        }
        s.d = 2;
        symList_2.push_back(s);
        return symList_2.size() - 1;
    }
    return -1;
}

int outCheck() {
    return 0;
}

void outSym(string s) {
    if (debug == 1) {
        outT << s << endl;
    }
}

instruct isCom(word w) {
    if (w == EQL) {
        return BNE;
    }
    if (w == NEQ) {
        return BEQ;
    }
    if (w == LEQ) {
        return BGTZ;
    }
    if (w == GRE) {
        return BLEZ;
    }
    if (w == GEQ) {
        return BLTZ;
    }
    if (w == LSS) {
        return BGEZ;
    }
    return ERROR_INS;
}

void error(char c) {
    if (c == 'a') {
        out << nNow << " a" << endl;
    } else if (c == 'k') {
        out << nline_1 << " k" << endl;
    } else {
        out << nline << " " << c << endl;
    }
}

void error(char c, symbol s) {

    out << s.n << " b" << endl;
}

void synAnalyse();

void program_analyse();

symbol str_analyse();

void const_ex_analyse();

void const_de_analyse();

int unsigned_analyse();

int int_analyse();

void head_analyse(symbol &s);

symbol const_analyse();

void var_ex_analyse();

void var_de_analyse();

void var_dis_analyse();

void var_ini_analyse();

void fun_re_analyse();

void fun_un_analyse();

void sen_mul_analyse();

void para_analyse(vector<dtype> &p);

void main_analyse();

dtype expre_analyse(symbol m);

//dtype expre_analyse();
dtype term_analyse(symbol m);

dtype factor_analyse(symbol m);

void sentence_analyse();

void sen_de_analyse();

void sen_if_analyse();

instruct if_analyse(symbol t1);

void sen_while_analyse();

symbol length_analyse();

void sen_switch_analyse();

void switch_list_analyse(dtype dt, symbol sEnd, symbol exp);

void sen_case_analyse(dtype dt, symbol sEnd, symbol exp);

void default_analyse();

dtype call_re_analyse();

void call_un_analyse();

void call_pa_analyse(symbol fun);

void sen_list_analyse();

void read_analyse();

void write_analyse();

void return_analyse();

void outMcode();

void mToMIPS();

void mipsPre();

void mipsOut();

symbol findS(string s) {
    int i;
    for (i = 0; i < symList_2.size(); i++) {
        if (symList_2[i].s == s) {
            return symList_2[i];
        }
    }
    for (i = 0; i < symList_1.size(); i++) {
        if (symList_1[i].s == s) {
            return symList_1[i];
        }
    }
    error('c');
    printf("Not found: ");
    cout << s << endl;
    symbol w = {.t = ERROR_T, .dt = ERROR_DT, .d = -1};
    return w;
}

int isMD(word w) {
    return (w == MULT || w == DIV);
}

int charType(char c) {
    int result = 0;
    if (c == '+' || c == '-' || c == '*' || c == '/'
        || isalpha(c) || isdigit(c) || c == '_') {
        result = 1; //char = 1
    } else if (c == 32 || c == 33 || (c <= 126 && c >= 35)) {
        result = 2; //string >= 1
    } else {
        result = 0;
    }
    return result;
}

int isoption(char c) {
    if (c == '<' || c == '>' || c == '!' || c == '=') {
        return 2;
    }
    if (c == '+' || c == '-' || c == '*' || c == ':'
        || c == ';' || c == '(' || c == ')' || c == '['
        || c == ']' || c == '{' || c == '}' || c == ','
        || c == '/') {
        return 1;
    }
    return 0;
}

void outA(word w, char s[]) {
    //out << wordS[w] << " " << s << endl;
    //fprintf(q, "%s %s\n", wordS[w], s);
    symput(s, w);
}

void outA(word w, char s[], int sn) {
    //out << wordS[w] << " " << s << endl;
    //fprintf(q, "%s %s\n", wordS[w], s);
    symput(s, w, sn);
}

int isPM(word w) {
    return (w == PLUS || w == MINU);
}

void outO(char c) {
    char s[2] = {c, 0};
    if (c == '+') {
        outA(PLUS, s);
        return;
    }
    if (c == '-') {
        outA(MINU, s);
        return;
    }
    if (c == '*') {
        outA(MULT, s);
        return;
    }
    if (c == '/') {
        outA(DIV, s);
        return;
    }
    if (c == '<') {
        outA(LSS, s);
        return;
    }
    if (c == '>') {
        outA(GRE, s);
        return;
    }
    if (c == ':') {
        outA(COLON, s);
        return;
    }
    if (c == '=') {
        outA(ASSIGN, s);
        return;
    }
    if (c == ';') {
        outA(SEMICN, s);
        return;
    }
    if (c == ',') {
        outA(COMMA, s);
        return;
    }
    if (c == '(') {
        outA(LPARENT, s);
        return;
    }
    if (c == ')') {
        outA(RPARENT, s);
        return;
    }
    if (c == '[') {
        outA(LBRACK, s);
        return;
    }
    if (c == ']') {
        outA(RBRACK, s);
        return;
    }
    if (c == '{') {
        outA(LBRACE, s);
        return;
    }
    if (c == '}') {
        outA(RBRACE, s);
        return;
    }
}

void outO2(char c1, char c2) {
    char s[3] = {c1, c2, 0};
    if (c1 == '<' && c2 == '=') {
        outA(LEQ, s);
    } else if (c1 == '>' && c2 == '=') {
        outA(GEQ, s);
    } else if (c1 == '=' && c2 == '=') {
        outA(EQL, s);
    } else if (c1 == '!' && c2 == '=') {
        outA(NEQ, s);
    }
}

void outWord(char s0[]) {
    char s[500];
    int i;
    for (i = 0; i < strlen(s0); i++) {
        if (isupper(s0[i])) {
            s[i] = s0[i] - 'A' + 'a';
        } else {
            s[i] = s0[i];
        }
    }
    s[i] = 0;
    if (strcmp(s, "const") == 0) {
        outA(CONSTTK, s0);
    } else if (strcmp(s, "int") == 0) {
        outA(INTTK, s0);
    } else if (strcmp(s, "char") == 0) {
        outA(CHARTK, s0);
    } else if (strcmp(s, "void") == 0) {
        outA(VOIDTK, s0);
    } else if (strcmp(s, "main") == 0) {
        outA(MAINTK, s0);
    } else if (strcmp(s, "if") == 0) {
        outA(IFTK, s0);
    } else if (strcmp(s, "else") == 0) {
        outA(ELSETK, s0);
    } else if (strcmp(s, "switch") == 0) {
        outA(SWITCHTK, s0);
    } else if (strcmp(s, "case") == 0) {
        outA(CASETK, s0);
    } else if (strcmp(s, "default") == 0) {
        outA(DEFAULTTK, s0);
    } else if (strcmp(s, "while") == 0) {
        outA(WHILETK, s0);
    } else if (strcmp(s, "for") == 0) {
        outA(FORTK, s0);
    } else if (strcmp(s, "scanf") == 0) {
        outA(SCANFTK, s0);
    } else if (strcmp(s, "printf") == 0) {
        outA(PRINTFTK, s0);
    } else if (strcmp(s, "return") == 0) {
        outA(RETURNTK, s0);
    } else {
        outA(IDENFR, s);
    }
}

void sym_out() {
    int i;
    cout << "dimension: 1" << endl;
    for (i = 0; i < symList_1.size(); i++) {
        symbol tem = symList_1[i];
        cout << symList_1[i].s << ", type: " << tem.t << ", data_type:" << tem.dt << endl;
    }
    cout << "dimension: 2" << endl;
    for (i = 0; i < symList_2.size(); i++) {
        symbol tem = symList_2[i];
        cout << symList_2[i].s << ", type: " << tem.t << ", data_type:" << tem.dt << endl;
    }
}

// mainF

void mCodeSimplify();

int main() {

    out.open("error.txt", ios::out);
    outT.open("test.txt", ios::out);
    outM.open("mCode.csv", ios::out);
    outL.open("mips.txt", ios::out);
    FILE *p = fopen("testfile.txt", "r");
    char c = fgetc(p);
    int i, j;
    char str[10000];

    while (c != 0) {
        if (c == EOF) {
            break;
        }
        if (c == '\n') {
            nNow += 1;
            c = fgetc(p);
            continue;
        }
        if (isspace(c)) {
            c = fgetc(p);
            continue;
        }
        if (isalpha(c) || c == '_') {
            i = 0;
            while (isalpha(c) || c == '_' || isdigit(c)) {
                str[i++] = c;
                c = fgetc(p);
            }
            str[i] = 0;
            outWord(str);
            continue;
        }
        if (isdigit(c)) {
            i = 0;
            while (isdigit(c)) {
                str[i++] = c;
                c = fgetc(p);
            }
            str[i] = 0;
            outA(INTCON, str);
            continue;
        }
        if (isoption(c) == 1) {
            outO(c);
            c = fgetc(p);
            continue;
        }
        if (isoption(c) == 2) {
            char c1 = c;
            c = fgetc(p);
            if (c == '=') {
                outO2(c1, c);
                c = fgetc(p);
                continue;
            }
            outO(c1);
            continue;
        }
        if (c == '\'') {
            c = fgetc(p);
            str[0] = c, str[1] = 0;
            outA(CHARCON, str, c);
            if (charType(c) != 1) {
                error('a');
            }
            while (c != '\'') {
                c = fgetc(p);
            }
            c = fgetc(p);
            continue;
        }
        if (c == '\"') {
            int strR = 1;
            i = 0;
            c = fgetc(p);
            if (c == '\"') {
                strR = 0;
            }
            while (c != '\"') {
                str[i++] = c;
                if (charType(c) == 0) {
                    strR = 0;
                }
                c = fgetc(p);
            }
            str[i] = 0;
            int numSt = addStr(str);
            if (strR == 0) {
                error('a');
                outA(STRCON, str, numSt);
            } else {
                outA(STRCON, str, numSt);
            }
            c = fgetc(p);
            continue;
        }
        printf("SOMETHING WRONG!!!!");
        break;
    }

    mipsPre();

    synAnalyse();

    mCodeSimplify();

    outMcode();

    mToMIPS();

    fclose(p);
    out.close();
    outL.close();
    outT.close();
    outM.close();
    //sym_out();
    return 0;
}

map<string, string> returnFun;
map<string, string> unreFun;

void synAnalyse() {
    sym = con[getT].type;
    getT++;
    program_analyse();
}

void program_analyse() {
    if (sym == CONSTTK) {
        const_ex_analyse();
    }
    if (isType(sym) && con[getT + 1].type != LPARENT) {
        var_ex_analyse();
    }
    addCode(TOMAIN, nu, nu, nu);
    while (1) {
        if (sym == VOIDTK && con[getT].type != MAINTK) {
            fun_un_analyse();
            continue;
        }
        if (isType(sym) && con[getT + 1].type == LPARENT) {
            fun_re_analyse();
            continue;
        }
        if (sym == VOIDTK && con[getT].type == MAINTK) {
            main_analyse();
            break;
        }
        wrong();
    }
    outSym("<程序>");
}

void main_analyse() {
    symbol all_end = {.s = "main"};
    funNameNow = "main";
    return_refer = all_end;
    funcT = VOID;
    // wrongG = 0;
    middle m = {.ins = MSP};
    addCode(m);
    dimension = 2;
    nowFun = 0;
    check(VOIDTK);
    check(MAINTK);
    check(LPARENT);
    check(RPARENT);
    check(LBRACE);
    sen_mul_analyse();
    check(RBRACE);
    dimension = 1;
    // sym_out();
    // symList_2.clear();
    mainA = nowFun;
    outSym("<主函数>");
}

void const_ex_analyse() {
    check(CONSTTK);
    const_de_analyse();
    check(SEMICN);

    while (1) {
        if (sym == CONSTTK) {
            symget();
            const_de_analyse();
            check(SEMICN);
            continue;
        }
        break;
    }
    outSym("<常量说明>");
}

void var_ex_analyse() {
    if (!isType(sym)) {
        wrong();
    }
    var_de_analyse();
    check(SEMICN);

    while (1) {
        if (isType(sym) && con[getT + 1].type != LPARENT) {
            var_de_analyse();
            check(SEMICN);
            continue;
        }
        break;
    }
    outSym("<变量说明>");
}

void fun_un_analyse() {
    funcT = VOID;
    //wrongG = 0;
    dimension = 2;
    nowFun = off_i;
    symbol temF = {.t = FUNC, .dt = VOID};
    temF.d = dimension;
    vector<dtype> p;
    check(VOIDTK);
    unreFun.insert(pair<string, string>(con[getT - 1].name, con[getT - 1].name));
    temF.s = con[getT - 1].name;
    funNameNow = temF.s;
    temF.n = con[getT - 1].n;

    check(IDENFR);
    check(LPARENT);
    para_analyse(p);
    temF.para = p;
    temF.v1 = p.size();
    dimension = 1;
    middle funin = {.ins = FUNIN, .s1 = temF};
    int fun_no = addCode(funin);
    int sym_no = symIn(temF);
    return_refer = temF;;
    dimension = 2;
    check(RPARENT);
    check(LBRACE);
    sen_mul_analyse();
    check(RBRACE);
    symList_2.clear();
    temF.addr = nowFun;
    symList_1[sym_no].addr = nowFun;
    mCode[fun_no].s1.addr = nowFun;
    middle funout = {.ins = FUNOUT, .s1 = temF};
    addCode(funout);
    outSym("<无返回值函数定义>");
}

void fun_re_analyse() {
    hasR = 0;
    //wrongG = 0;
    dimension = 2;
    nowFun = off_i;
    symbol temF = {.t = FUNC};
    head_analyse(temF);
    funcT = temF.dt;
    vector<dtype> p;
    check(LPARENT);
    para_analyse(p);
    dimension = 1;
    temF.para = p;
    temF.v1 = p.size();
    middle funin = {.ins = FUNIN, .s1 = temF};
    int fun_no = addCode(funin);
    int sym_no = symIn(temF);
    return_refer = temF;
    dimension = 2;
    check(RPARENT);
    check(LBRACE);
    sen_mul_analyse();
    if (hasR == 0) {
        error('h');
    }
    check(RBRACE);
    symList_2.clear();
    temF.addr = nowFun;
    mCode[fun_no].s1.addr = nowFun;
    symList_1[sym_no].addr = nowFun;
    middle funout = {.ins = FUNOUT, .s1 = temF};
    addCode(funout);
    outSym("<有返回值函数定义>");
}

void head_analyse(symbol &s) {
    if (!isType(sym)) {
        wrong();
    }
    s.dt = sym == CHARTK ? CHAR : INT;
    symget();
    s.s = con[getT - 1].name;

    funNameNow = s.s;

    s.n = con[getT - 1].n;
    returnFun.insert(pair<string, string>(con[getT - 1].name, con[getT - 1].name));
    check(IDENFR);
    outSym("<声明头部>");
}

void const_de_analyse() {
    symbol tem = {.t = CONST};
    if (sym == INTTK) {
        tem.dt = INT;
        check(INTTK);

        tem.s = con[getT - 1].name;
        tem.n = con[getT - 1].n;
        check(IDENFR);
        check(ASSIGN);
        tem.value = int_analyse();
        symIn(tem);
        while (1) {
            symbol tem2 = {.t = CONST, .dt = INT};
            if (sym == COMMA) {
                symget();
                tem2.n = con[getT - 1].n;
                tem2.s = con[getT - 1].name;
                check(IDENFR);
                check(ASSIGN);
                tem2.value = int_analyse();
                symIn(tem2);
                continue;
            }
            break;
        }
    } else if (sym == CHARTK) {
        tem.dt = CHAR;
        check(CHARTK);

        tem.s = con[getT - 1].name;
        tem.n = con[getT - 1].n;
        check(IDENFR);
        check(ASSIGN);
        tem.value = con[getT - 1].sn;
        check(CHARCON);
        symIn(tem);
        while (1) {
            symbol tem2 = {.t = CONST, .dt = CHAR};
            if (sym == COMMA) {
                symget();
                tem2.s = con[getT - 1].name;
                tem2.n = con[getT - 1].n;

                check(IDENFR);
                check(ASSIGN);
                tem2.value = con[getT - 1].sn;
                check(CHARCON);
                symIn(tem2);
                continue;
            }
            break;
        }
    }
    outSym("<常量定义>");
}

int int_analyse() {
    int tt = 0;
    if (sym == PLUS || sym == MINU) {
        if (sym == MINU) {
            tt = 1;
        }
        symget();
    }
    int r = unsigned_analyse();
    outSym("<整数>");
    return tt == 0 ? r : -r;
}

int unsigned_analyse() {
    //printf("%s!!!\n", con[getT + 1].name);
    int r = stoi(con[getT - 1].name);
    check(INTCON);
    outSym("<无符号整数>");
    return r;
}

void var_de_analyse() {
    dtype temT;
    int e = 0;
    if (!isType(sym)) {
        wrong();
    }
    temT = sym == CHARTK ? CHAR : INT;
    symget();
    int tem100 = 1;
    int ifDe = 0;
    do {
        symbol tem = {.dt = temT};
        tem.d = dimension;
        if (tem100 == 1) {
            tem.s = con[getT - 1].name;
            tem.n = con[getT - 1].n;
            check(IDENFR);
            tem100 = 0;
        } else {
            if (sym == COMMA) {
                symget();
                tem.n = con[getT - 1].n;
                tem.s = con[getT - 1].name;
                check(IDENFR);
            } else {
                break;
            }
        }

        if (sym == ASSIGN) {
            middle m = {.ins = TO};
            tem.t = VAR;
            symIn(tem);
            m.result = tem;
            symget();

            m.s1 = const_analyse();

            if (m.s1.dt != temT) {
                error('o');
            }
            addCode(m);
            ifDe = 1;
            break;
        } else if (sym == LBRACK) {
            int d1, d2, i1, i2;

            tem.t = ARRAY;
            tem.ww = 1;
            symget();
            d1 = unsigned_analyse();
            tem.d2_t = d1;
            tem.d = 1;
            tem.d1 = 1;
            tem.d2 = d1;

            check(RBRACK);

            if (sym == ASSIGN) {

                symIn(tem);
                tem.d1 = 0;
                tem.d2 = 0;
                middle m = {.ins = AWD};
                symget();
                symbol varT;
                check(LBRACE);
                varT = const_analyse();
                if (varT.dt != temT) {
                    error('o');
                }
                m.result = tem;
                m.s1 = varT;
                addCode(m);
                i1 = 1;
                while (1) {
                    if (sym != COMMA) {
                        break;
                    } else {
                        i1++;
                        symget();
                        varT = const_analyse();
                        if (varT.dt != temT) {
                            error('o');
                        }
                        tem.d2++;
                        m.result = tem;
                        m.s1 = varT;
                        addCode(m);
                    }
                }
                if (i1 != d1) {
                    e = 1;
                }
                check(RBRACE);
                ifDe = 1;
                break;
            } else if (sym == LBRACK) {
                tem.ww = 2;
                tem.d = 2;
                symget();
                d2 = unsigned_analyse();
                tem.d2_t = d2;
                tem.d1 = d1;
                tem.d2 = d2;
                symIn(tem);
                check(RBRACK);
                tem.d1 = 0;
                tem.d2 = 0;
                if (sym == ASSIGN) {
                    middle m = {.ins = AWD};
                    symbol varT;
                    symget();
                    check(LBRACE);
                    check(LBRACE);
                    varT = const_analyse();
                    if (varT.dt != temT) {
                        error('o');
                    }
                    m.result = tem;
                    m.s1 = varT;
                    addCode(m);
                    i2 = 1;
                    while (1) {
                        if (sym != COMMA) {
                            break;
                        } else {
                            tem.d2++;
                            i2++;
                            symget();
                            varT = const_analyse();
                            if (varT.dt != temT) {
                                error('o');
                            }
                            m.result = tem;
                            m.s1 = varT;
                            addCode(m);
                        }
                    }

                    if (d2 != i2) {
                        e = 1;
                    }
                    check(RBRACE);
                    i1 = 1;
                    while (1) {
                        if (sym != COMMA) {
                            break;
                        } else {
                            tem.d1++;
                            tem.d2 = 0;
                            i1++;
                            symget();
                            check(LBRACE);
                            varT = const_analyse();
                            if (varT.dt != temT) {
                                error('o');
                            }
                            m.result = tem;
                            m.s1 = varT;
                            addCode(m);
                            i2 = 1;
                            while (1) {
                                if (sym != COMMA) {
                                    break;
                                } else {
                                    tem.d2++;
                                    i2++;
                                    symget();
                                    varT = const_analyse();
                                    if (varT.dt != temT) {
                                        error('o');
                                    }
                                    m.result = tem;
                                    m.s1 = varT;
                                    addCode(m);
                                }
                            }
                            if (d2 != i2) {
                                e = 1;
                            }
                            check(RBRACE);
                        }
                    }
                    if (d1 != i1) {
                        e = 1;
                    }
                    check(RBRACE);
                    ifDe = 1;
                    break;
                } else {
                    continue;
                }
            } else {
                symIn(tem);
            }
        } else {
            symIn(tem);
            continue;
        }
    } while (1);

    if (e == 1) {
        error('n');
    }

    if (ifDe) {
        outSym("<变量定义及初始化>");
    } else {
        outSym("<变量定义无初始化>");
    }
    outSym("<变量定义>");
}

symbol const_analyse() {
    symbol s = {.t = CONST};
    if (sym == CHARCON) {
        s.dt = CHAR;
        s.value = con[getT - 1].sn;
        symget();
    } else {
        s.dt = INT;
        s.value = int_analyse();
    }
    outSym("<常量>");
    return s;
}

void sen_mul_analyse() {
    if (sym == CONSTTK) {
        const_ex_analyse();
    }
    if (isType(sym) && con[getT + 1].type != LPARENT) {
        var_ex_analyse();
    }
    sen_list_analyse();
    outSym("<复合语句>");
}

void para_analyse(vector<dtype> &dt) {
    if (!isType(sym)) {
        outSym("<参数表>");
        return;
    }
    symbol tem;
    tem.d = 2;
    if (sym == CHARTK) {
        tem.dt = CHAR;
        dt.push_back(CHAR);
    } else if (sym == INTTK) {
        tem.dt = INT;
        dt.push_back(INT);
    }
    symget();
    string s = con[getT - 1].name;
    tem.n = con[getT - 1].n;
    tem.s = s;
    tem.t = VAR;
    check(IDENFR);
    symIn(tem);
    while (1) {
        symbol tem2 = {.t = VAR};
        tem2.d = 2;
        if (sym == COMMA) {
            symget();
            if (!isType(sym)) {
                wrong();
            }

            if (sym == CHARTK) {
                tem2.dt = CHAR;
                dt.push_back(CHAR);
            } else if (sym == INTTK) {
                dt.push_back(INT);
                tem2.dt = INT;
            }

            symget();
            string s2 = con[getT - 1].name;
            tem2.n = con[getT - 1].n;
            tem2.s = s2;
            symIn(tem2);
            check(IDENFR);
            continue;
        }
        break;
    }
    outSym("<参数表>");
}

void sen_list_analyse() {
    while (1) {
        if (sym == WHILETK || sym == FORTK || sym == IFTK ||
            sym == IDENFR || sym == SCANFTK || sym == PRINTFTK ||
            sym == SWITCHTK || sym == SEMICN || sym == RETURNTK ||
            sym == LBRACE) {
            sentence_analyse();
            continue;
        }
        break;
    }
    outSym("<语句列>");
}

void sentence_analyse() {
    if (sym == WHILETK || sym == FORTK) {
        sen_while_analyse();
    } else if (sym == IFTK) {
        sen_if_analyse();
    } else if (sym == IDENFR) {
        string strT1;
        strT1 = con[getT - 1].name;
        word temW100 = con[getT].type;
        if (temW100 == ASSIGN || temW100 == LBRACK) {
            sen_de_analyse();
        } else if (temW100 == LPARENT) {
            if (unreFun.find(strT1) != unreFun.end()) {
                call_un_analyse();
            } else if (returnFun.find(strT1) != returnFun.end()) {
                call_re_analyse();
            } else {
                call_un_analyse();
            }
        }
        check(SEMICN);
    } else if (sym == SCANFTK) {
        read_analyse();
        check(SEMICN);
    } else if (sym == PRINTFTK) {
        write_analyse();
        check(SEMICN);
    } else if (sym == SWITCHTK) {
        sen_switch_analyse();
    } else if (sym == SEMICN) {
        check(SEMICN);
    } else if (sym == RETURNTK) {
        return_analyse();
        check(SEMICN);
    } else if (sym == LBRACE) {
        symget();
        sen_list_analyse();
        check(RBRACE);
    } else {
        wrong();
    }
    outSym("<语句>");
}

void sen_de_analyse() {
    string s = con[getT - 1].name;
    symbol sR = findS(s);
    if (sR.t == CONST) {
        error('j');
    }
    symbol s1 = newTemp();
    middle m = {.result = sR, .s1 = s1, .s2 = zero};
    check(IDENFR);

    if (sym == ASSIGN) {
        m.ins = TO;
        check(ASSIGN);
        expre_analyse(s1);
    } else if (sym == LBRACK) {
        m.ins = AW;
        symget();
        dtype temT;
        symbol temD1 = newTemp();
        temT = expre_analyse(temD1);
        if (temT == CHAR) {
            error('i');
        }
        check(RBRACK);
        if (sym == ASSIGN) {
            m.s3 = temD1;
            symget();
            expre_analyse(s1);
        } else if (sym == LBRACK) {
            symbol temD2 = newTemp();
            symget();
            temT = expre_analyse(temD2);
            if (temT == CHAR) {
                error('i');
            }
            m.s2 = temD1, m.s3 = temD2;
            check(RBRACK);
            check(ASSIGN);
            expre_analyse(s1);
        } else {
            wrong();
        }
    } else {
        wrong();
    }
    addCode(m);
    outSym("<赋值语句>");
}

void sen_if_analyse() {
    symbol i1 = newIndex(); // 跳到else
    symbol t1 = newTemp();
    middle m1 = {.result = i1, .s1 = t1};
    check(IFTK);
    check(LPARENT);
    m1.ins = if_analyse(t1);
    addCode(m1);
    check(RPARENT);
    sentence_analyse();
    symbol i2 = newIndex(); // 跳到结束
    middle m2 = {.ins = J, .result = i2};
    addCode(m2);
    middle index1 = {.ins = IND, .s1 = i1};
    addCode(index1);
    if (sym == ELSETK) {
        symget();
        sentence_analyse();
    }
    index1.s1 = i2;
    addCode(index1);
    outSym("<条件语句>");
}

instruct if_analyse(symbol t1) {
    symbol e1 = newTemp();
    symbol e2 = newTemp();
    instruct i;
    dtype temT = expre_analyse(e1);
    if (temT == CHAR) {
        error('f');
    }
    if ((i = isCom(sym)) != ERROR_INS) {
        symget();
    } else {
        wrong();
        symget();
    }
    temT = expre_analyse(e2);
    if (temT == CHAR) {
        error('f');
    }
    addCode(MI, t1, e1, e2);
    outSym("<条件>");
    return i;
}

void sen_while_analyse() {
    if (sym == WHILETK) {
        symbol ba = newIndex();
        middle mj = {.ins = IND, .s1 = ba};
        addCode(mj);
        symbol i = newIndex();
        symbol t = newTemp();
        middle m = {.result = i, .s1 = t};
        check(WHILETK);
        check(LPARENT);
        m.ins = if_analyse(t);
        addCode(m);
        check(RPARENT);
        sentence_analyse();
        middle j = {.ins = J, .result = ba};
        addCode(j);
        mj.s1 = i;
        addCode(mj);
    } else if (sym == FORTK) {
        check(FORTK);
        check(LPARENT);
        symbol i1 = newIndex(), i2 = newIndex();
        middle in1 = {.ins = IND, .s1 = i1};
        middle in2 = {.ins = IND, .s1 = i2};
        string s = con[getT - 1].name;
        symbol tem1 = findS(s);
        if (tem1.t == CONST) {
            error('j');
        }
        symbol tem2 = newTemp();
        check(IDENFR);
        check(ASSIGN);
        expre_analyse(tem2);
        addCode(TO, tem1, tem2, nu);
        addCode(in1);
        symbol tem3 = newTemp();

        middle m = {.result = i2, .s1 = tem3};

        check(SEMICN);
        m.ins = if_analyse(tem3);
        check(SEMICN);
        addCode(m);
        s = con[getT - 1].name;
        symbol tem4 = findS(s);
        if (tem4.t == CONST) {
            error('j');
        }
        check(IDENFR);
        check(ASSIGN);
        s = con[getT - 1].name;
        symbol tem5 = findS(s);
        check(IDENFR);
        middle ins1 = {.result = tem4, .s1 = tem5};
        if (sym == PLUS) {
            ins1.ins = PLU;
            symget();
        } else if (sym == MINU) {
            ins1.ins = MI;
            symget();
        } else {
            wrong();
            symget();
        }
        symbol tem6 = length_analyse();
        ins1.s2 = tem6;
        check(RPARENT);
        sentence_analyse();
        addCode(ins1);
        middle j = {.ins = J, .result = i1};
        addCode(j);
        addCode(in2);
    }
    outSym("<循环语句>");
}

symbol length_analyse() {
    symbol r = {.t = CONST, .dt = INT};
    r.value = unsigned_analyse();
    outSym("<步长>");
    return r;
}

void sen_switch_analyse() {
    symbol sEnd = newIndex();
    check(SWITCHTK);
    check(LPARENT);
    symbol exp = newTemp();
    dtype dt = expre_analyse(exp);
    check(RPARENT);
    check(LBRACE);
    switch_list_analyse(dt, sEnd, exp);
    default_analyse();
    check(RBRACE);
    middle end_index = {.ins = IND, .s1 = sEnd};
    addCode(end_index);
    outSym("<情况语句>");
}

void switch_list_analyse(dtype dt, symbol sEnd, symbol exp) {
    sen_case_analyse(dt, sEnd, exp);
    while (sym == CASETK) {
        sen_case_analyse(dt, sEnd, exp);
    }
    outSym("<情况表>");
}

void sen_case_analyse(dtype dt, symbol sEnd, symbol exp) {
    check(CASETK);
    symbol cEnd = newIndex();
    symbol t1 = newTemp();
    middle all_end = {.ins = J, .result = sEnd};
    middle ifIn = {.ins = BNE, .result = cEnd, .s1 = t1};
    symbol t2 = const_analyse();
    if (t2.dt != dt) {
        error('o');
    }
    addCode(MI, t1, exp, t2);
    addCode(ifIn);
    check(COLON);
    sentence_analyse();

    addCode(all_end);

    middle i_end = {.ins = IND, .s1 = cEnd};
    addCode(i_end);
    outSym("<情况子语句>");
}

void default_analyse() {
    if (check(DEFAULTTK) == 0) {
        error('p');
        return;
    }
    check(COLON);
    sentence_analyse();
    outSym("<缺省>");
}

dtype call_re_analyse() {
    string s = con[getT - 1].name;
    symbol fun = findS(s);
    dtype re = fun.dt;
    middle call = {.ins = CALL, .s1 = fun};
    addCode(call);
    check(IDENFR);
    check(LPARENT);
    call_pa_analyse(fun);
    check(RPARENT);

    middle mP = {.ins = PARATO};
    for (int t = fun.v1 - 1; t >= 0; t--) {
        int tt = 4 * t + off_i;
        mP.d = tt;
        symbol sP = para_stack.top();

        mP.s1 = sP;

        para_stack.pop();
        addCode(mP);
        // PARATO s1 = sP, p = tt
    }

    call.ins = CEND;
    addCode(call);
    outSym("<有返回值函数调用语句>");
    return re;
}

void call_un_analyse() {
    string s = con[getT - 1].name;
    symbol fun = findS(s);
    middle call = {.ins = CALL, .s1 = fun};
    addCode(call);
    check(IDENFR);
    check(LPARENT);
    call_pa_analyse(fun);
    check(RPARENT);

    middle mP = {.ins = PARATO};

    for (int t = fun.v1 - 1; t >= 0; t--) {
        int tt = 4 * t + off_i;
        mP.d = tt;
        symbol sP = para_stack.top();

        mP.s1 = sP;

        para_stack.pop();
        addCode(mP);
        // PARATO s1 = sP, p = tt
    }

    call.ins = CEND;
    addCode(call);
    outSym("<无返回值函数调用语句>");
}

void call_pa_analyse(symbol tem) {
    int off = off_i;
    // middle m = {.ins = PARATO, .d = off};
    off += 4;
    int err = 0;
    if (tem.d == -1) {
        err = 1;
    }
    int i = 0;
    dtype d;
    int e = 0;
    if (sym == IDENFR || isPM(sym) || sym == LPARENT ||
        sym == INTCON || sym == CHARCON) {
        symbol tem1 = newTemp();

        para_stack.push(tem1);

        d = expre_analyse(tem1);
        // m.s1 = tem1;
        // addCode(m);
        if (i < tem.v1 && d != tem.para[i]) {
            e = 1;
        }
        i++;
        while (sym == COMMA) {
            check(COMMA);
            tem1 = newTemp();

            para_stack.push(tem1);

            d = expre_analyse(tem1);
            // m.s1 = tem1;
            // m.d = off;
            off += 4;
            //addCode(m);
            if (i < tem.v1 && d != tem.para[i]) {
                e = 1;
            }
            i++;
        }
    }
    if (err == 0) {
        if (i != tem.v1) {
            error('d');
        } else if (e == 1) {
            error('e');
        }
    }
    outSym("<值参数表>");
}

void read_analyse() {
    check(SCANFTK);
    check(LPARENT);
    string s = con[getT - 1].name;
    if (findS(s).t == CONST) {
        error('j');
    }
    symbol s1 = findS(con[getT - 1].name);
    if (s1.dt == CHAR) {
        addCode(SCHAR, s1, nu, nu);
    } else if (s1.dt == INT) {
        addCode(SINT, s1, nu, nu);
    }
    check(IDENFR);
    check(RPARENT);
    outSym("<读语句>");
}

void write_analyse() {
    check(PRINTFTK);
    check(LPARENT);
    if (sym == STRCON) {
        addCode(PSTR, nu, str_analyse(), nu);
        if (sym == COMMA) {
            symget();
            symbol s = newTemp();
            dtype dtt = expre_analyse(s);
            if (dtt == CHAR) {
                addCode(PCHAR, nu, s, nu);
            } else if (dtt == INT) {
                addCode(PINT, nu, s, nu);
            }
        }
    } else {
        symbol s = newTemp();
        dtype dtt = expre_analyse(s);
        if (dtt == CHAR) {
            addCode(PCHAR, nu, s, nu);
        } else if (dtt == INT) {
            addCode(PINT, nu, s, nu);
        }
    }
    addCode(PN, nu, nu, nu);
    check(RPARENT);
    outSym("<写语句>");
}

void return_analyse() {
    check(RETURNTK);
    symbol t1 = newTemp();
    middle m = {.ins = RET, .result = return_refer, .s1 = t1};
    if (sym == LPARENT) {
        if (funcT == VOID) {
            error('g');
        }
        check(LPARENT);
        if (con[getT - 1].type == RPARENT || con[getT - 1].type == SEMICN) {
            if (funcT == CHAR || funcT == INT)
                error('h');
        } else {
            dtype tem = expre_analyse(t1);
            if (tem != funcT) {
                if (funcT == CHAR || funcT == INT)
                    error('h');
            }
        }
        check(RPARENT);
    } else {
        if (funcT != VOID) {
            error('h');
        }
    }
    hasR = 1;
    addCode(m);
    outSym("<返回语句>");
}

dtype expre_analyse(symbol s1) {
    //out << "!!" << wordS[sym] << " " << con[getT - 1].name << endl;
    dtype re = CHAR;
    symbol s2;
    int pm = 0; // 0 + 1 -
    if (isPM(sym)) {
        re = INT;
        if (sym == MINU) {
            pm = 1;
        }
        symget();
    }
    dtype d2 = term_analyse(s1);

    if (pm == 1) {
        addCode(MI, s1, zero, s1);
    }

    re = re == INT ? INT : d2;
    if (isPM(sym)) {
        s2 = newTemp();
    }
    while (isPM(sym)) {
        re = INT;
        if (sym == MINU) {
            pm = 1;
        } else {
            pm = 0;
        }
        symget();
        term_analyse(s2);
        if (pm == 1) {
            addCode(MI, s1, s1, s2);
        } else {
            addCode(PLU, s1, s1, s2);
        }
    }
    outSym("<表达式>");
    return re;
}

dtype term_analyse(symbol s) {
    //out << "!!" << wordS[sym] << " " << con[getT - 1].name << endl;
    dtype re = CHAR;
    symbol s2;
    re = factor_analyse(s);
    if (isMD(sym)) {
        s2 = newTemp();
    }
    while (isMD(sym)) {
        int md = sym == MULT ? 0 : 1; // 0 * 1 /
        re = INT;
        symget();
        factor_analyse(s2);
        if (md == 0) {
            addCode(MU, s, s, s2);
        } else {
            addCode(DI, s, s, s2);
        }
    }
    outSym("<项>");
    return re;
}

dtype factor_analyse(symbol ss) {
    dtype re;
    if (sym == IDENFR && con[getT].type == LPARENT) {
        re = call_re_analyse();
        addCode(GETR, ss, nu, nu);
    } else if (sym == IDENFR) {
        string s = con[getT - 1].name;
        symbol tem = findS(s);
        symget();
        if (tem.d != -1) {
            re = tem.dt;
        }
        if (sym == LBRACK) {
            symbol t1 = newTemp();
            middle m = {.ins = AR, .result = ss, .s1 = tem, .s2 = zero, .s3 = t1};
            symget();
            dtype temT = expre_analyse(t1);
            if (temT == CHAR) {
                error('i');
            }
            check(RBRACK);
            if (sym == LBRACK) {
                symbol t2 = newTemp();
                m.s2 = t1, m.s3 = t2;
                symget();
                temT = expre_analyse(t2);
                if (temT == CHAR) {
                    error('i');
                }
                check(RBRACK);
            }
            addCode(m);
        } else {
            addCode(TO, ss, tem, zero);
        }
    } else if (sym == LPARENT) {
        re = INT;
        check(LPARENT);
        expre_analyse(ss);
        check(RPARENT);
    } else if (sym == CHARCON) {
        re = CHAR;
        symbol c1 = {.t = CONST};
        c1.value = con[getT - 1].sn;
        addCode(TO, ss, c1, zero);
        check(CHARCON);
    } else if (isPM(sym) || sym == INTCON) {
        re = INT;
        symbol i1 = {.t = CONST};
        i1.value = int_analyse();
        addCode(TO, ss, i1, zero);
    }
    outSym("<因子>");
    return re;
}

symbol str_analyse() {
    symbol s = {.t = STR, .value = con[getT - 1].sn};
    check(STRCON);
    outSym("<字符串>");
    return s;
}

void outMcode() {
    outM << "instruct,result,type1,s1,value1,type2,s2,value2,s3,value3" << endl;
    for (int i = 0; i < mCode.size(); i++) {
        outM << instructC[mCode[i].ins] << "," << mCode[i].result.s << "," << type_out[mCode[i].s1.t] << ","
             << mCode[i].s1.s << "," << mCode[i].s1.value << ","
             << type_out[mCode[i].s2.t] << "," << mCode[i].s2.s << "," << mCode[i].s2.value << "," << mCode[i].s3.s
             << "," << mCode[i].s3.value << endl;
    }
}

void mToMIPS() {

    symbol mainFun = {.t = FUNC, .s = "main", .addr = mainA};

    symList_1.push_back(mainFun);

    mipsOut();
    outL << "main_endof: " << endl;

}

void mipsPre() {
    outL << ".data" << endl;

    string reg1("\\\\");
    regex r1(reg1);

    for (int i = 0; i < strs.size(); i++) {
        string reg2("\\\\");
        string after_reg = regex_replace(strs[i], r1, reg2);
        outL << "    str" << i << ": .asciiz \"" << after_reg << "\"" << endl;
        offset += strs[i].length() + 1;
    }
    outL << "    strn" << ": .asciiz \"\\n\"" << endl;
    outL << ".text" << endl;
    offset += 4 - offset % 4;
}

void removeTemp(symbol s) {
    for (int i = 0; i < constTem.size(); i++) {
        if (s.s == constTem[i].s) {
            constTem[i].s = "thisisjustadisusedOne";
            return;
        }
    }
}

void load(symbol s, string str, string fun) {
    if (isConst(s)) {
        outL << "    li $" << str << ", " << s.value << endl;
        return;
    }
    if (s.d == 1) {
        outL << "    lw $" << str << ", " << addrOf(s) << endl;
    } else if (s.d == 2) {
        int off = findS(fun).addr - s.addr;
        outL << "    lw $" << str << ", " << off << "($sp)" << endl;
    }
}

void load(symbol s, string str, string fun, int off) {
    if (s.d == 1) {
        outL << "    lw $" << str << ", " << addrOf(s) << "($t4)" << endl;
    } else if (s.d == 2) {
        int offs = findS(fun).addr - s.addr;
        outL << "    subu $t4, $sp, $t4" << endl;
        outL << "    lw $" << str << ", " << offs << "($t4)" << endl;
    }
}

void loadK(symbol s, string str, string fun, int off) {
    if (s.d == 1) {
        int offs = addrOf(s) + off;
        outL << "    lw $" << str << ", " << offs << endl;
    } else if (s.d == 2) {
        int offs = findS(fun).addr - s.addr;
        offs -= off;
        outL << "    lw $" << str << ", " << offs << "($sp)" << endl;
    }
}

void save(symbol s, string str, string fun) {
    if (s.d == 1) {
        outL << "    sw $" << str << ", " << addrOf(s) << endl;
    } else if (s.d == 2) {
        int off = findS(fun).addr - s.addr;
        outL << "    sw $" << str << ", " << off << "($sp)" << endl;
    }
}

void saveK(symbol s, string str, string fun, int off) {
    if (s.d == 1) {
        int offs = addrOf(s) + off;
        outL << "    sw $" << str << ", " << offs << endl;
    } else if (s.d == 2) {
        int offs = findS(fun).addr - s.addr;
        offs -= off;
        outL << "    sw $" << str << ", " << offs << "($sp)" << endl;
    }
}

void save(symbol s, string str, string fun, int off) {
    if (s.d == 1) {
        outL << "    sw $" << str << ", " << addrOf(s) << "($t4)" << endl;
    } else if (s.d == 2) {
        int offs = findS(fun).addr - s.addr;
        outL << "    subu $t4, $sp, $t4" << endl;
        outL << "    sw $" << str << ", " << offs << "($t4)" << endl;
    }
}

void toT0(symbol s, string fun) {
    load(s, "t0", fun);
}

void toT1(symbol s, string fun) {
    load(s, "t1", fun);
}

void mipsOut() {
    int i;
    int c1, c2, cr, fin, bi;
    for (i = 0; i < mCode.size(); i++) {
        fin = 0;
        middle m = mCode[i];
        outL << "# " << instructC[m.ins] << " s1: " << m.s1.s << " || result: " << m.result.s << " || func: " << m.fun
             << endl;

        if (m.ins == PINT) {
            load(m.s1, "a0", m.fun);
            outL << "    li $v0, 1" << endl;
            outL << "    syscall" << endl;
        } else if (m.ins == PCHAR) {
            outL << "    li $v0, 11" << endl;
            load(m.s1, "a0", m.fun);
            outL << "    syscall" << endl;
        } else if (m.ins == PSTR) {
            outL << "    li $v0, 4" << endl;
            outL << "    la $a0, str" << m.s1.value << endl;
            outL << "    syscall" << endl;
        } else if (m.ins == PN) {
            outL << "    li $v0, 4" << endl;
            outL << "    la $a0, strn" << endl;
            outL << "    syscall" << endl;
        } else if (m.ins == SINT) {
            outL << "    li $v0, 5" << endl;
            outL << "    syscall" << endl;
            save(m.result, "v0", m.fun);
        } else if (m.ins == SCHAR) {
            outL << "    li $v0, 12" << endl;
            outL << "    syscall" << endl;
            save(m.result, "v0", m.fun);
            // outL << "    sw $v0, " << addrOf(m.result) << endl;
        } else if (m.ins == TO) {
            if (isConst(m.s1)) {
                if (getConst(m.s1) == 0) {
                    fin = 1;
                    save(m.result, "zero", m.fun);
                }
            }
            if (fin == 0) {
                toT0(m.s1, m.fun);
                save(m.result, "t0", m.fun);
            }
        } else if (m.ins == PLU) {
            if (isConst(m.s1, m.s2)) {
                cr = getConst(m.s1) + getConst(m.s2);
                outL << "    li $t0, " << cr << endl;
                save(m.result, "t0", m.fun);
                fin = 1;
            } else if (isConst(m.s1)) {
                fin = 1;
                c1 = getConst(m.s1);
                load(m.s2, "t0", m.fun);
                if (c1 != 0) {
                    outL << "    addu $t0, $t0, " << c1 << endl;
                }
                save(m.result, "t0", m.fun);
            } else if (isConst(m.s2)) {
                fin = 1;
                c1 = getConst(m.s2);
                load(m.s1, "t0", m.fun);
                if (c1 != 0) {
                    outL << "    addu $t0, $t0, " << c1 << endl;
                }
                save(m.result, "t0", m.fun);
            }
            if (fin == 0) {
                toT0(m.s1, m.fun);
                toT1(m.s2, m.fun);
                outL << "    addu $t0, $t0, $t1" << endl;
                save(m.result, "t0", m.fun);
            }
            // outL << "    sw $t0, " << addrOf(m.result) << endl;
        } else if (m.ins == MI) {
            if (isConst(m.s1, m.s2)) {
                cr = getConst(m.s1) - getConst(m.s2);
                outL << "    li $t0, " << cr << endl;
                save(m.result, "t0", m.fun);
                fin = 1;
            } else if (isConst(m.s1)) {
                fin = 1;
                c1 = getConst(m.s1);
                load(m.s2, "t0", m.fun);
                outL << "    subu $t0, $zero, $t0" << endl;
                if (c1 != 0) {
                    outL << "    addu $t0, $t0, " << c1 << endl;
                }
                save(m.result, "t0", m.fun);
            } else if (isConst(m.s2)) {
                fin = 1;
                c1 = getConst(m.s2);
                load(m.s1, "t0", m.fun);
                c1 = -c1;
                if (c1 != 0) {
                    outL << "    addu $t0, $t0, " << c1 << endl;
                }
                save(m.result, "t0", m.fun);
            }

            if (fin == 0) {
                toT0(m.s1, m.fun);
                toT1(m.s2, m.fun);
                outL << "    subu $t0, $t0, $t1" << endl;
                save(m.result, "t0", m.fun);
            }
            // outL << "    sw $t0, " << addrOf(m.result) << endl;
        } else if (m.ins == MU) {
            if (isConst(m.s1, m.s2)) {
                cr = getConst(m.s1) * getConst(m.s2);
                outL << "    li $t0, " << cr << endl;
                save(m.result, "t0", m.fun);
                fin = 1;
            } else if (isConst(m.s1)) {
                c1 = getConst(m.s1);
                bi = binary2(c1);
                if (c1 == 0) {
                    fin = 1;
                    save(m.result, "zero", m.fun);
                } else if (bi < 100) {
                    fin = 1;
                    if (bi == 0) {
                        load(m.s2, "t0", m.fun);
                        save(m.result, "t0", m.fun);
                    } else if (bi < 0) {
                        bi = -bi;
                        load(m.s2, "t0", m.fun);
                        outL << "sll $t0, $t0, " << bi << endl;
                        outL << "subu $t0, $zero, $t0" << endl;
                        save(m.result, "t0", m.fun);
                    } else {
                        load(m.s2, "t0", m.fun);
                        outL << "sll $t0, $t0, " << bi << endl;
                        save(m.result, "t0", m.fun);
                    }
                }
            } else if (isConst(m.s2)) {
                c1 = getConst(m.s2);
                bi = binary2(c1);
                cout << m.s2.s << "  " << bi << "  " << m.s2.value << endl;
                if (c1 == 0) {
                    fin = 1;
                    save(m.result, "zero", m.fun);
                } else if (bi < 100) {
                    fin = 1;
                    if (bi == 0) {
                        load(m.s1, "t0", m.fun);
                        save(m.result, "t0", m.fun);
                    } else if (bi < 0) {
                        bi = -bi;
                        load(m.s1, "t0", m.fun);
                        outL << "sll $t0, $t0, " << bi << endl;
                        outL << "subu $zero, $t0" << endl;
                        save(m.result, "t0", m.fun);
                    } else {
                        load(m.s1, "t0", m.fun);
                        outL << "sll $t0, $t0, " << bi << endl;
                        save(m.result, "t0", m.fun);
                    }
                }
            }
            if (fin == 0) {
                toT0(m.s1, m.fun);
                toT1(m.s2, m.fun);
                outL << "    mult $t0, $t1" << endl;
                outL << "    mflo $t0" << endl;
                save(m.result, "t0", m.fun);
            }

            // outL << "    sw $t0, " << addrOf(m.result) << endl;
        } else if (m.ins == DI) {
            if (isConst(m.s1, m.s2)) {
                cr = getConst(m.s1) / getConst(m.s2);
                outL << "    li $t0, " << cr << endl;
                save(m.result, "t0", m.fun);
                fin = 1;
            } else if (isConst(m.s1)) {
                c1 = getConst(m.s1);
                if (c1 == 0) {
                    fin = 1;
                    save(m.result, "zero", m.fun);
                }
            } else if (isConst(m.s2)) {
                c1 = getConst(m.s2);
                bi = binary2(c1);
                if (bi < 50) {
                    if (bi == 0) {
                        fin = 1;
                        load(m.s1, "t0", m.fun);
                        save(m.result, "t0", m.fun);
                    }
                }
            }
            if (fin == 0) {
                toT0(m.s1, m.fun);
                toT1(m.s2, m.fun);
                outL << "    div $t0, $t1" << endl;
                outL << "    mflo $t0" << endl;
                save(m.result, "t0", m.fun);
            }
            // outL << "    sw $t0, " << addrOf(m.result) << endl;
        } else if (m.ins == J) {
            outL << "    j " << m.result.s << endl;
        } else if (m.ins == BEQ) {
            toT0(m.s1, m.fun);
            outL << "    beq $t0, $zero, " << m.result.s << endl;
        } else if (m.ins == BNE) {
            toT0(m.s1, m.fun);
            outL << "    bne $t0, $zero, " << m.result.s << endl;
        } else if (m.ins == BLTZ) {
            toT0(m.s1, m.fun);
            outL << "    bltz $t0, " << m.result.s << endl;
        } else if (m.ins == BLEZ) {
            toT0(m.s1, m.fun);
            outL << "    blez $t0, " << m.result.s << endl;
        } else if (m.ins == BGTZ) {
            toT0(m.s1, m.fun);
            outL << "    bgtz $t0, " << m.result.s << endl;
        } else if (m.ins == BGEZ) {
            toT0(m.s1, m.fun);
            outL << "    bgez $t0, " << m.result.s << endl;
        } else if (m.ins == IND) {
            outL << m.s1.s << ":" << endl;
        } else if (m.ins == AR) {
            int d2 = m.s1.d2_t;

            if (isConst(m.s2, m.s3)) {
                c1 = getConst(m.s2);
                c2 = getConst(m.s3);
                cr = (d2 * c1 + c2) << 2;
                cout << "read array: " << m.s1.s << " of offset: " << cr << endl;
                loadK(m.s1, "t5", m.fun, cr);
                fin = 1;
                save(m.result, "t5", m.fun);
            } else if (isConst(m.s2)) {
                fin = 1;
                c1 = getConst(m.s2);
                cr = (d2 * c1);
                if (cr == 0) {
                    load(m.s3, "t4", m.fun);
                    outL << "    sll $t4, $t4, 2" << endl;
                    load(m.s1, "t5", m.fun, 1);
                    save(m.result, "t5", m.fun);
                } else {
                    load(m.s3, "t4", m.fun);
                    outL << "    addiu $t4, $t4, " << cr << endl;
                    outL << "    sll $t4, $t4, 2" << endl;
                    load(m.s1, "t5", m.fun, 1);
                    save(m.result, "t5", m.fun);
                }
            } else if (isConst(m.s3)) {
                fin = 1;
                c1 = getConst(m.s3);
                load(m.s2, "t0", m.fun);
                outL << "    li $t2, " << d2 << endl;
                outL << "    mult $t0, $t2" << endl;
                outL << "    mflo $t4" << endl;
                if (c1 == 0) {
                    outL << "    sll $t4, $t4, 2" << endl;
                    load(m.s1, "t5", m.fun, 1);
                    save(m.result, "t5", m.fun);
                } else {
                    outL << "    addiu $t4, $t4, " << c1 << endl;
                    outL << "    sll $t4, $t4, 2" << endl;
                    load(m.s1, "t5", m.fun, 1);
                    save(m.result, "t5", m.fun);
                }
            }

            if (fin == 0) {
                load(m.s2, "t0", m.fun);
                load(m.s3, "t1", m.fun);
                outL << "    li $t2, " << d2 << endl;
                outL << "    mult $t0, $t2" << endl;
                outL << "    mflo $t4" << endl;
                outL << "    addu $t4, $t4, $t1" << endl;
                outL << "    sll $t4, $t4, 2" << endl;
                load(m.s1, "t5", m.fun, 1);
                save(m.result, "t5", m.fun);
            }
        } else if (m.ins == AWD) {
            int off = (m.result.d1 * m.result.d2_t + m.result.d2) * 4;
            load(m.s1, "t5", m.fun);
            saveK(m.result, "t5", m.fun, off);
        } else if (m.ins == AW) {

            int d2 = m.result.d2_t;

            if (isConst(m.s2, m.s3)) {
                c1 = getConst(m.s2);
                c2 = getConst(m.s3);
                cr = (d2 * c1 + c2) << 2;
                load(m.s1, "t5", m.fun);
                fin = 1;
                saveK(m.result, "t5", m.fun, cr);
            } else if (isConst(m.s2)) {
                fin = 1;
                c1 = getConst(m.s2);
                cr = (d2 * c1);
                if (cr == 0) {
                    load(m.s3, "t4", m.fun);
                    outL << "    sll $t4, $t4, 2" << endl;
                    load(m.s1, "t5", m.fun);
                    save(m.result, "t5", m.fun, 1);
                } else {
                    load(m.s3, "t4", m.fun);
                    outL << "    addiu $t4, $t4, " << cr << endl;
                    outL << "    sll $t4, $t4, 2" << endl;
                    load(m.s1, "t5", m.fun);
                    save(m.result, "t5", m.fun, 1);
                }
            } else if (isConst(m.s3)) {
                fin = 1;
                c1 = getConst(m.s3);
                load(m.s2, "t0", m.fun);
                outL << "    li $t2, " << d2 << endl;
                outL << "    mult $t0, $t2" << endl;
                outL << "    mflo $t4" << endl;
                if (c1 == 0) {
                    outL << "    sll $t4, $t4, 2" << endl;
                    load(m.s1, "t5", m.fun);
                    save(m.result, "t5", m.fun, 1);
                } else {
                    outL << "    addiu $t4, $t4, " << c1 << endl;
                    outL << "    sll $t4, $t4, 2" << endl;
                    load(m.s1, "t5", m.fun);
                    save(m.result, "t5", m.fun, 1);
                }
            }

            if (fin == 0) {
                load(m.s2, "t0", m.fun);
                load(m.s3, "t1", m.fun);

                outL << "    li $t2, " << d2 << endl;
                outL << "    mult $t0, $t2" << endl;
                outL << "    mflo $t4" << endl;
                outL << "    addu $t4, $t4, $t1" << endl;
                outL << "    sll $t4, $t4, 2" << endl;
                load(m.s1, "t5", m.fun);
                save(m.result, "t5", m.fun, 1);
            }
        } else if (m.ins == FUNIN) {
            outL << m.s1.s << ": " << endl;

            outL << "    addiu $sp, $sp, -" << findS(m.s1.s).addr << endl;

        } else if (m.ins == FUNOUT) {
            outL << m.s1.s << "_endof: " << endl;
            outL << "    move $a2, $ra" << endl;

            int offs = findS(m.fun).addr;
            outL << "    lw $ra, " << offs << "($sp)" << endl;
            //outL << "    addiu $t3, $t3, " << offs << endl;
            outL << "    lw $sp, " << offs - 4 << "($sp)" << endl;

            outL << "    jr $a2" << endl;
        } else if (m.ins == RET) {
            load(m.s1, "a1", m.fun);
            outL << "    j " << m.result.s << "_endof" << endl;
        } else if (m.ins == GETR) {
            // outL << "# DEBUG: " << m.result.s << " di: " << m.result.d << endl;
            save(m.result, "a1", m.fun);
        } else if (m.ins == CALL) {

        } else if (m.ins == PARATO) {

            load(m.s1, "t2", m.fun);
            outL << "    sw $t2, -" << m.d << "($sp)" << endl;

        } else if (m.ins == CEND) {

            outL << "    sw $ra, 0($sp)" << endl;
            outL << "    sw $sp, -4($sp)" << endl;

            outL << "    jal " << m.s1.s << endl;

        } else if (m.ins == MSP) {
            outL << "main:" << endl;
            outL << "    addiu $sp, $sp, -" << mainA << endl;

            outL << "    move $t3, $sp" << endl;

        } else if (m.ins == TOMAIN) {
            outL << "    j main" << endl;
        } else if (m.ins == NON) {
            continue;
        }
    }
}

int isTemp(symbol s) {
    return s.t == TEMP || s.t == CONST_TEM;
}

void mCodeSimplify() {
    int c1, c2, cr;

    for (int i = 0; i < mCode.size(); i++) {
        middle &m = mCode[i];

        updateTem(m.s1);
        updateTem(m.s2);
        updateTem(m.s3);

        if (m.ins == TO) {
            if (isConst(m.s1) && isTemp(m.result)) {
                c1 = getConst(m.s1);

                m.ins = NON;
                m.result.t = CONST_TEM;
                m.result.value = c1;
                addTem(m.result);
            } else if (m.result.t == TEMP) {
                // m.result.t = TEMP;
                removeTemp(m.result);
            }
        } else if (m.ins == PLU) {
            if (isConst(m.s1, m.s2) && isTemp(m.result)) {
                c1 = getConst(m.s1);
                c2 = getConst(m.s2);
                cr = c1 + c2;

                m.ins = NON;
                m.result.t = CONST_TEM;
                m.result.value = cr;
                addTem(m.result);
            } else if (m.result.t == TEMP) {
                // m.result.t = TEMP;
                removeTemp(m.result);
            }
        } else if (m.ins == MI) {
            if (isConst(m.s1, m.s2) && isTemp(m.result)) {
                c1 = getConst(m.s1);
                c2 = getConst(m.s2);
                cr = c1 - c2;

                m.ins = NON;
                m.result.t = CONST_TEM;
                m.result.value = cr;
                addTem(m.result);
            } else if (m.result.t == TEMP) {
                // m.result.t = TEMP;
                removeTemp(m.result);
            }
        } else if (m.ins == MU) {
            if (isConst(m.s1, m.s2) && isTemp(m.result)) {
                c1 = getConst(m.s1);
                c2 = getConst(m.s2);
                cr = c1 * c2;

                m.ins = NON;
                m.result.t = CONST_TEM;
                m.result.value = cr;
                addTem(m.result);
            } else if (m.result.t == TEMP) {
                // m.result.t = TEMP;
                removeTemp(m.result);
            }
        } else if (m.ins == DI) {
            if (isConst(m.s1, m.s2) && isTemp(m.result)) {
                c1 = getConst(m.s1);
                c2 = getConst(m.s2);
                cr = c1 / c2;

                m.ins = NON;
                m.result.t = CONST_TEM;
                m.result.value = cr;
                addTem(m.result);
            } else if (m.result.t == TEMP) {
                // m.result.t = TEMP;
                removeTemp(m.result);
            }
        } else if (m.ins == GETR) {
            if (m.result.t == TEMP) {
                removeTemp(m.result);
            }
        } else if (m.ins == AR) {
            if (m.result.t == TEMP) {
                removeTemp(m.result);
            }
        }
    }
}
