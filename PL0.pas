program pl0 ;  { version 1.0 oct.1989 }
{ PL/0 compiler with code generation }	
{常量定义}
const norw = 13;          { no. of reserved words }	{保留字的数目}
      txmax = 100;        { length of identifier table }	{符号表长度}
      nmax = 14;          { max. no. of digits in numbers }	{数字的最大长度}
      al = 10;            { length of identifiers }	{标识符的最大长度}
      amax = 2047;        { maximum address }	{相对地址最大值}
      levmax = 3;         { maximum depth of block nesting } 	{最大嵌套层数}
      cxmax = 200;        { size of code array }	{生成目标代码数组最大长度}

{类型变量定义}
type symbol =
     ( nul,ident,number,plus,minus,times,slash,oddsym,eql,neq,lss,
       leq,gtr,geq,lparen,rparen,comma,semicolon,period,becomes,
       beginsym,endsym,ifsym,thensym,whilesym,dosym,callsym,constsym,
       varsym,procsym,readsym,writesym );	{symbol的宏定义为一个枚举}
     alfa = packed array[1..al] of char;	{alfa宏定义为含有a1个元素的合并数组，为标识符的类型}
     objecttyp = (constant,variable,prosedure);		{objecttyp的宏定义为一个枚举}
     symset = set of symbol;	{symset为symbol的集合}
     fct = ( lit,opr,lod,sto,cal,int,jmp,jpc,red,wrt ); { functions }	{fct为一个枚举，其实是PCODE的各条指令}
     instruction = packed record	{instruction声明为一个记录类型}
                     f : fct;            { function code }	{函数代码}
                     l : 0..levmax;      { level }	{嵌套层次}
                     a : 0..amax;        { displacement address }	{相对位移地址}
                   end;
                  {   lit 0, a : load constant a	读取常量a到数据栈栈顶
                      opr 0, a : execute operation a	执行a运算
                      lod l, a : load variable l,a	读取变量放到数据栈栈顶，变量的相对地址为a，层次差为1
                      sto l, a : store variable l,a	将数据栈栈顶内容存入变量，变量的相对地址为a，层次差为1
                      cal l, a : call procedure a at level l	调用过程，过程入口指令为a,层次差为1
                      int 0, a : increment t-register by a	数据栈栈顶指针增加a
                      jmp 0, a : jump to a	无条件跳转到指令地址a
                      jpc 0, a : jump conditional to a	条件转移到指令地址a
                      red l, a : read variable l,a	读数据并存入变量，
                      wrt 0, 0 : write stack-top	将栈顶内容输出
                  }

{全局变量定义}
var   ch : char;      { last character read }	{最后读出的字符}
      sym: symbol;    { last symbol read }	{最近识别出来符号类型}
      id : alfa;      { last identifier read }	{最后读出来的识别符}
      num: integer;   { last number read }	{最后读出来的数字}
      cc : integer;   { character count }	{行缓冲区指针}
      ll : integer;   { line length }	{行缓冲区长度}
      kk,err: integer;	
      cx : integer;   { code allocation index }	{代码分配指针}
      line: array[1..81] of char;	{缓冲一行代码}
      a : alfa;	{用来存储symbol的变量}
      code : array[0..cxmax] of instruction;	{用来保存编译后的PCODE代码，最大容量为cxmax}
      word : array[1..norw] of alfa;	{保留字表}
      wsym : array[1..norw] of symbol	{保留字表中每个保留字对应的symbol类型}
      ssym : array[char] of symbol;		{符号对应的symbol类型}
      mnemonic : array[fct] ofW	{助记符}
                   packed array[1..5] of char;
      declbegsys, statbegsys, facbegsys : symset;	{声明开始，表达式开始、项开始的符号集合}
      table : array[0..txmax] of	{定义符号表}
                record	{表中的元素类型是记录类型}
                  name : alfa;	{元素名}
                  case kind: objecttyp of	{根据符号的类型保存相应的信息}
                    constant : (val:integer );	{如果是常量，val中保存常量的值}
                    variable,prosedure: (level,adr: integer )	{如果是变量或过程，保存存放层数和偏移地址}
                end;
      fin : text;     { source program file }	{源代码文件}
      sfile: string;  { source program file name }	{源程序文件名}

procedure error( n : integer );  {错误处理程序}
  begin
    writeln( '****', ' ':cc-1, '^', n:2 );	{报错提示信息，'^'指向出错位置，并提示错误类型}
    err := err+1 {错误次数+1}
  end; { error }

procedure getsym;	{词法分析程序}
var i,j,k : integer;	{声明计数变量}
procedure getch;
	begin
      if cc = ll  { get character to end of line }	{如果读完了一行（行指针与该行长度相等）}
      then begin { read next line }	{开始读取下一行}
             if eof(fin)	{如果到达文件末尾}
             then begin
                   writeln('program incomplete');	{报错}
                   close(fin);	{关闭文件}
                   exit;	{退出}
                  end;
             ll := 0;	{将行长度重置}
             cc := 0;	{将行指针重置}
             write(cx:4,' ');  { print code address }	{输出代码地址，宽度为4}
             while not eoln(fin) do	{当没有到行末时}
               begin
                 ll := ll+1;	{将行缓冲区的长度+1}
                 read(fin,ch);	{从文件中读取一个字符到ch中}
                 write(ch);	{控制台输出ch}
                 line[ll] := ch	{把这个字符放到当前行末尾}
               end;
             writeln;	{换行}
             readln(fin);	{源文件读取从下一行开始}
             ll := ll+1;	{行长度计数加一}
             line[ll] := ' ' { process end-line }	{行数组最后一个元素为空格}
           end;
      cc := cc+1;	{行指针+1}
      ch := line[cc]	{读取下一个字符，将字符放进全局变量ch}
    end; { getch }
  begin { procedure getsym;   }	{标识符识别开始}
    while ch = ' ' do	{去除空字符}
      getch;	{调用上面的getch过程}
    if ch in ['a'..'z']	{如果识别到字母，那么有可能是保留字或标识符}
    then begin  { identifier of reserved word }	{开始识别}
           k := 0;	{标识符指针置零，这个量用来统计标识符长度}
           repeat	{循环}
             if k < al	{如果k的大小小于标识符的最大长度}
             then begin
                   k := k+1;	{k++}
                   a[k] := ch	{将ch写入标识符暂存变量a}
                 end;
             getch	{获取下一个字符}
           until not( ch in ['a'..'z','0'..'9']);	{直到读出的不是数字或字母的时候，标识符结束}
           if k >= kk        { kk : last identifier length }	{若k比kk大}
           then kk := k	{kk记录当前标识符的长度k}
           else repeat	{循环}
                  a[kk] := ' ';		{标识符最后一位为空格}
                  kk := kk-1	{k--}
               until kk = k;	{直到kk等于当前标识符的长度，这样做的意义是防止上一个标识符存在a中的内容影响到当前标识符，比如上一个标识符为“qwerty”，现在的标识符为“abcd”，如果不清后几位则a中会保存"abcdty"，这显然是错误的}
           id := a;	{id保存标识符名}
           i := 1;	{i指向第一个保留字}
           j := norw;   { binary search reserved word table }	{二分查找保留字表，将j设为保留字的最大数目}
           repeat
             k := (i+j) div 2;	{再次用到k，但这里只是作为二分查找的中间变量}
             if id <= word[k]	{若当前标识符小于或等于保留字表中的第k个，这里的判断依据的是字典序，那么我们可以推测符号表是按照字典序保存的}
             then j := k-1;		{j = k-1}
             if id >= word[k]	{若当前标识符大于或等于保留字表中的第k个}
             then i := k+1		{i = k+1}
           until i > j;		{查找结束条件}
           if i-1 > j	{找到了}
           then sym := wsym[k]	{将找到的保留字类型赋给sym}
           else sym := ident	{未找到则把sym职位ident类型，表示是标识符}
         end
    else if ch in ['0'..'9']	{如果字符是数字}
         then begin  { number }
                k := 0;	{这里的k用来记录数字的位数}
                num := 0;	{num保存数字}
                sym := number;	{将标识符设置为数字}
                repeat	{循环开始}
                  num := 10*num+(ord(ch)-ord('0'));	{将数字字符转换为数字并拼接起来赋给num}
                  k := k+1;	{k++}
                  getch	{继续读字符}
                until not( ch in ['0'..'9']);	{直到输入的不再是数字}
                if k > nmax	{如果数字的位数超过了数字允许的最大长度}
                then error(30)	{报错}
              end
	else if ch = ':'	{当字符不是数字或字母，而是':'时}
		 then begin
				getch;	{读下一个字符}
				if ch = '='	{如果下一个字符是'='}
				then begin
					  sym := becomes;	{将标识符sym设置为becomes，表示复制}
					  getch	{读下一个字符}
					end
				else sym := nul {否则，将标识符设置为nul，表示非法}
			   end
	else if ch = '<'	{当读到的字符是'<'时}
		   then begin	
				  getch;	{读下一个字符}
				  if ch = '='	{若读到的字符是'='}
				  then begin
						 sym := leq;	{则sym为leq,表示小于等于}
						 getch	{读下一个字符}
					   end
				  else if ch = '>'	{若读到的字符是'>'}
					   then begin
							 sym := neq;	{则sym为neq,表示不等于}
							 getch	{读下一个字符}
						   end
				  else sym := lss	{否则,sym设为lss,表示小于}
				end
	else if ch = '>'	{若读到的是'>'}
			then begin
				   getch;	{读下一个字符}
				   if ch = '='	{若读到的是'='}
				   then begin
						  sym := geq;	{sym设为geq,表示大于等于}
						  getch	{读下一个字符}
						end
				   else sym := gtr	{否则,sym设为gtr,表示大于}
				 end
	else begin	{若非上述几种符号}
		   sym := ssym[ch];	{从ssym表中查到此字符对应的类型,赋给sym}
		   getch	{读下一个字符}
		 end
	end; { getsym }

procedure gen( x: fct; y,z : integer ); 
  begin
    if cx > cxmax
    then begin
           writeln('program too long');
           close(fin);
           exit
         end;
    with code[cx] do
      begin
        f := x;
        l := y;
        a := z
      end;
    cx := cx+1
  end; { gen }

procedure test( s1,s2 :symset; n: integer ); 
  begin
    if not ( sym in s1 )
    then begin
           error(n);
           s1 := s1+s2;
           while not( sym in s1) do
             getsym
           end
  end; { test }

procedure block( lev,tx : integer; fsys : symset ); 
  var  dx : integer;  { data allocation index }
       tx0: integer;  { initial table index }
       cx0: integer;  { initial code index }

  procedure enter( k : objecttyp ); 
    begin  { enter object into table }
      tx := tx+1;
      with table[tx] do
        begin
          name := id;
          kind := k;
          case k of
            constant : begin
                      if num > amax
                      then begin
                            error(30);
                            num := 0
                           end;
                      val := num
                    end;
            variable : begin
                      level := lev;
                      adr := dx;
                      dx := dx+1
                    end;
            prosedure: level := lev;
          end
        end
    end; { enter }

function position ( id : alfa ): integer; 
  var i : integer;
  begin
    table[0].name := id;
    i := tx;
    while table[i].name <> id do
       i := i-1;
    position := i
  end;  { position }

procedure constdeclaration; 
    begin
      if sym = ident
      then begin
             getsym;
             if sym in [eql,becomes]
             then begin
                    if sym = becomes
                    then error(1);
                    getsym;  
                    if sym = number
                    then begin
                           enter(constant);
                           getsym
                         end
                    else error(2)
                  end
             else error(3)
           end
      else error(4)
    end; { constdeclaration }

  procedure vardeclaration; 
    begin
      if sym = ident
      then begin
             enter(variable);
             getsym
           end
      else error(4)
    end; { vardeclaration }

  procedure listcode;  
    var i : integer;
    begin
      for i := cx0 to cx-1 do
        with code[i] do
          writeln( i:4, mnemonic[f]:7,l:3, a:5)
    end; { listcode }

procedure statement( fsys : symset );
var i,cx1,cx2: integer;
procedure expression( fsys: symset); 
      var addop : symbol;
      procedure term( fsys : symset);  
        var mulop: symbol ;
        procedure factor( fsys : symset );
          var i : integer;
          begin
            test( facbegsys, fsys, 24 );
            while sym in facbegsys do
              begin
                if sym = ident
                then begin
                       i := position(id);
                       if i= 0
                       then error(11)
                       else
                         with table[i] do
                           case kind of
                             constant : gen(lit,0,val);
variable : gen(lod,lev-level,adr);
prosedure: error(21)
                           end;
                       getsym
                     end
                else if sym = number
                     then begin
                            if num > amax
                            then begin
                                   error(30);
                                   num := 0
                                 end;
                            gen(lit,0,num);
                            getsym
                          end
                     else if sym = lparen
                          then begin
                                 getsym;
                                 expression([rparen]+fsys);
                                 if sym = rparen
                                 then getsym
                                 else error(22)
                               end;
                test(fsys,[lparen],23)
              end
          end; { factor }
        begin { procedure term( fsys : symset);   
                var mulop: symbol ;    }
          factor( fsys+[times,slash]);
          while sym in [times,slash] do
            begin
              mulop := sym;
              getsym;
              factor( fsys+[times,slash] );
              if mulop = times
              then gen( opr,0,4 )
              else gen( opr,0,5)
            end
        end; { term }
      begin { procedure expression( fsys: symset);  
              var addop : symbol; }
        if sym in [plus, minus]
        then begin
               addop := sym;
               getsym;
               term( fsys+[plus,minus]);
               if addop = minus
               then gen(opr,0,1)
             end
        else term( fsys+[plus,minus]);
        while sym in [plus,minus] do
          begin
            addop := sym;
            getsym;
            term( fsys+[plus,minus] );
            if addop = plus
            then gen( opr,0,2)
            else gen( opr,0,3)
          end
      end; { expression }

    procedure condition( fsys : symset ); 
      var relop : symbol;
      begin
        if sym = oddsym
        then begin
               getsym;
               expression(fsys);
               gen(opr,0,6)
             end
        else begin
             expression( [eql,neq,lss,gtr,leq,geq]+fsys);
             if not( sym in [eql,neq,lss,leq,gtr,geq])
               then error(20)
               else begin
                      relop := sym;
                      getsym;
                      expression(fsys);
                      case relop of
                        eql : gen(opr,0,8);
                        neq : gen(opr,0,9);
                        lss : gen(opr,0,10);
                        geq : gen(opr,0,11);
                        gtr : gen(opr,0,12);
                        leq : gen(opr,0,13);
                      end
                    end
             end
      end; { condition }
    begin { procedure statement( fsys : symset );  
      var i,cx1,cx2: integer; }
      if sym = ident
      then begin
             i := position(id);
             if i= 0
             then error(11)
             else if table[i].kind <> variable
                  then begin { giving value to non-variation }
                         error(12);
                         i := 0
                       end;
             getsym;
             if sym = becomes
             then getsym
             else error(13);
             expression(fsys);
             if i <> 0
             then
               with table[i] do
                  gen(sto,lev-level,adr)
          end
      else if sym = callsym
      then begin
             getsym;
             if sym <> ident
             then error(14)
             else begin
                    i := position(id);
                    if i = 0
                    then error(11)
                    else
                      with table[i] do
                        if kind = prosedure
                        then gen(cal,lev-level,adr)
                        else error(15);
                    getsym
                  end
           end
      else if sym = ifsym
           then begin
                  getsym;
                  condition([thensym,dosym]+fsys);
                  if sym = thensym
                  then getsym
                  else error(16);
                  cx1 := cx;
                  gen(jpc,0,0);
                  statement(fsys);
                  code[cx1].a := cx
                end
           else if sym = beginsym
                then begin
                       getsym;
                       statement([semicolon,endsym]+fsys);
                       while sym in ([semicolon]+statbegsys) do
                         begin
                           if sym = semicolon
                           then getsym
                           else error(10);
                           statement([semicolon,endsym]+fsys)
                         end;
                       if sym = endsym
                       then getsym
                       else error(17)
                     end
                else if sym = whilesym
                     then begin
                            cx1 := cx;
                            getsym;
                            condition([dosym]+fsys);
                            cx2 := cx;
                            gen(jpc,0,0);
                            if sym = dosym
                            then getsym
                            else error(18);
                            statement(fsys);
                            gen(jmp,0,cx1);
                            code[cx2].a := cx
                          end
                     else if sym = readsym
                          then begin
                                 getsym;
                                 if sym = lparen
                                 then
                                   repeat
                                     getsym;
                                     if sym = ident
                                     then begin
                                            i := position(id);
                                            if i = 0
                                            then error(11)
                                            else if table[i].kind <> variable
                                                 then begin
                                                        error(12);
                                                        i := 0
                                                      end
                                                 else with table[i] do
                                                       gen(red,lev-level,adr)
                                         end
                                     else error(4);
                                     getsym;
                                   until sym <> comma
                                 else error(40);
                                 if sym <> rparen
                                 then error(22);
                                 getsym
                               end
                          else if sym = writesym
                               then begin
                                      getsym;
                                      if sym = lparen
                                      then begin
                                             repeat
                                               getsym;
                                               expression([rparen,comma]+fsys);
                                               gen(wrt,0,0);
                                             until sym <> comma;
                                             if sym <> rparen
                                             then error(22);
                                             getsym
                                           end
                                      else error(40)
                                    end;
      test(fsys,[],19)
    end; { statement }
  begin  {   procedure block( lev,tx : integer; fsys : symset );   
    var  dx : integer;  /* data allocation index */
    tx0: integer;  /*initial table index */
    cx0: integer;  /* initial code index */              }
    dx := 3;
    tx0 := tx;
    table[tx].adr := cx;
    gen(jmp,0,0); { jump from declaration part to statement part }
    if lev > levmax
    then error(32);

    repeat
      if sym = constsym
      then begin
             getsym;
             repeat
               constdeclaration;
               while sym = comma do
                 begin
                   getsym;
                   constdeclaration
                 end;
               if sym = semicolon
               then getsym
               else error(5)
             until sym <> ident
           end;
      if sym = varsym
      then begin
             getsym;
             repeat
               vardeclaration;
               while sym = comma do
                 begin
                   getsym;
                   vardeclaration
                 end;
               if sym = semicolon
               then getsym
               else error(5)
             until sym <> ident;
           end;
      while sym = procsym do
        begin
          getsym;
          if sym = ident
          then begin
                 enter(prosedure);
                 getsym
               end
          else error(4);
          if sym = semicolon
          then getsym
          else error(5);
          block(lev+1,tx,[semicolon]+fsys);
          if sym = semicolon
          then begin
                 getsym;
                 test( statbegsys+[ident,procsym],fsys,6)
               end
          else error(5)
        end;
      test( statbegsys+[ident],declbegsys,7)
    until not ( sym in declbegsys );
    code[table[tx0].adr].a := cx;  { back enter statement code's start adr. }
    with table[tx0] do
      begin
        adr := cx; { code's start address }
      end;
    cx0 := cx;
    gen(int,0,dx); { topstack point to operation area }
    statement( [semicolon,endsym]+fsys);
    gen(opr,0,0); { return }
    test( fsys, [],8 );
    listcode;
  end { block };

procedure interpret;  
  const stacksize = 500;
  var p,b,t: integer; { program-,base-,topstack-register }
     i : instruction;{ instruction register }
     s : array[1..stacksize] of integer; { data store }
  function base( l : integer ): integer;
    var b1 : integer;
    begin { find base l levels down }
      b1 := b;
      while l > 0 do
        begin
          b1 := s[b1];
          l := l-1
        end;
      base := b1
    end; { base }
  begin  
    writeln( 'START PL/0' );
    t := 0;
    b := 1;
    p := 0;
    s[1] := 0;
    s[2] := 0;
    s[3] := 0;
    repeat
      i := code[p];
      p := p+1;
      with i do
        case f of
          lit : begin
                  t := t+1;
                  s[t]:= a;
              end;
          opr : case a of { operator }
                  0 : begin { return }
                        t := b-1;
                        p := s[t+3];
                        b := s[t+2];
                     end;
                  1 : s[t] := -s[t];
                  2 : begin
                        t := t-1;
                        s[t] := s[t]+s[t+1]
                     end;
                  3 : begin
                        t := t-1;
                        s[t] := s[t]-s[t+1]
                     end;
                  4 : begin
                        t := t-1;
                        s[t] := s[t]*s[t+1]
                     end;
                  5 : begin
                        t := t-1;
                        s[t] := s[t]div s[t+1]
                     end;
                  6 : s[t] := ord(odd(s[t]));
                  8 : begin
                        t := t-1;
                        s[t] := ord(s[t]=s[t+1])
                    end;
                  9 : begin
                        t := t-1;
                        s[t] := ord(s[t]<>s[t+1])
                     end;
                  10: begin
                        t := t-1;
                        s[t] := ord(s[t]< s[t+1])
                     end;
                  11: begin
                        t := t-1;
                        s[t] := ord(s[t] >= s[t+1])
                     end;
                  12: begin
                        t := t-1;
                        s[t] := ord(s[t] > s[t+1])
                     end;
                  13: begin
                        t := t-1;
                        s[t] := ord(s[t] <= s[t+1])
                     end;
                end;
          lod : begin
                  t := t+1;
                  s[t] := s[base(l)+a]
              end;
          sto : begin
                  s[base(l)+a] := s[t];  { writeln(s[t]); }
                  t := t-1
              end;
          cal : begin  { generate new block mark }
                  s[t+1] := base(l);
                  s[t+2] := b;
                  s[t+3] := p;
                  b := t+1;
                  p := a;
              end;
          int : t := t+a;
          jmp : p := a;
          jpc : begin
                  if s[t] = 0
                  then p := a;
                  t := t-1;
              end;
          red : begin
                  writeln('??:');
                  readln(s[base(l)+a]); 
              end;
          wrt : begin
                  writeln(s[t]);
                  t := t+1
              end
        end { with,case }
    until p = 0;
    writeln('END PL/0');
  end; { interpret }

begin { main }	{ 主函数 }
  writeln('please input source program file name : ');	{  }
  readln(sfile);
  assign(fin,sfile);
  reset(fin);
  for ch := 'A' to ';' do
    ssym[ch] := nul;
  word[1] := 'begin        '; word[2] := 'call         ';
  word[3] := 'const        '; word[4] := 'do           ';
  word[5] := 'end          '; word[6] := 'if           ';
  word[7] := 'odd          '; word[8] := 'procedure    ';
  word[9] := 'read         '; word[10]:= 'then         ';
  word[11]:= 'var          '; word[12]:= 'while        ';
  word[13]:= 'write        ';

  wsym[1] := beginsym;      wsym[2] := callsym;
  wsym[3] := constsym;      wsym[4] := dosym;
  wsym[5] := endsym;        wsym[6] := ifsym;
  wsym[7] := oddsym;        wsym[8] := procsym;
  wsym[9] := readsym;       wsym[10]:= thensym;
  wsym[11]:= varsym;        wsym[12]:= whilesym;
  wsym[13]:= writesym;

  ssym['+'] := plus;        ssym['-'] := minus;
  ssym['*'] := times;       ssym['/'] := slash;
  ssym['('] := lparen;      ssym[')'] := rparen;
  ssym['='] := eql;         ssym[','] := comma;
  ssym['.'] := period;
  ssym['<'] := lss;         ssym['>'] := gtr;
  ssym[';'] := semicolon;

  mnemonic[lit] := 'LIT  '; mnemonic[opr] := 'OPR  ';
  mnemonic[lod] := 'LOD  '; mnemonic[sto] := 'STO  ';
  mnemonic[cal] := 'CAL  '; mnemonic[int] := 'INT  ';
  mnemonic[jmp] := 'JMP  '; mnemonic[jpc] := 'JPC  ';
  mnemonic[red] := 'RED  '; mnemonic[wrt] := 'WRT  ';

  declbegsys := [ constsym, varsym, procsym ];
  statbegsys := [ beginsym, callsym, ifsym, whilesym];
  facbegsys := [ ident, number, lparen ];
  err := 0;
  cc := 0;
  cx := 0;
  ll := 0;
  ch := ' ';
  kk := al;
  getsym;
  block( 0,0,[period]+declbegsys+statbegsys );
  if sym <> period
  then error(9);
  if err = 0
  then interpret
  else write('ERRORS IN PL/0 PROGRAM');
  writeln;
  close(fin)
  readln(sfile);
end.           