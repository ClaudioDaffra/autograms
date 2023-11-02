unit U_SelfDerscribingSentences;
{Copyright © 2010, 2015  Gary Darby,  www.DelphiForFun.org
 This program may be used or modified for any non-commercial purpose
 so long as this original notice remains in place.
 All other rights are reserved
 }

{
The Search button will look for solutions for the "self
describing" sentence above the row of asterisks.   The
objective is to make the sentence true after number words
(one, five,  thirteen, etc.) replace the underscore characters.

You can replace the sentence with your own which may or may
not have a solution.  Increasing the number letter counts
rquired will reduce the chance of finding a solution.  The
number word range is from "one"  to
"ninetynine". The program recognizes letters to search based
on finding the pattern " X's" (space-letter-apostrophe-s)
preceeded by at least one underscore character.  For the
vowel or consonant search, it looks for the words "vowels" or
"consonants" preceeded by one or more spaces preceeded by one
or more underscore characters.
}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  shellAPI, StdCtrls, ExtCtrls, strutils, jpeg, dffutils;
type
  TLetterrec=record
    letter:char; {letter to search for}
    position:integer; {where in the text the request occurs}
    InCount:integer;  {how many of that letter appear in text}
    testvalue:integer;  {the trial number, (the current target value)}
    testword:string;  {its word equivalent}
    testcount:integer;  {the count of trial number values found}
  end;


  TForm1 = class(TForm)
    StaticText1: TStaticText;
    Panel1: TPanel;
    SearchBtn: TButton;
    Memo1: TMemo;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Memo2: TMemo;
    Memo4: TMemo;
    Memo5: TMemo;
    Label3: TLabel;
    procedure StaticText1Click(Sender: TObject);
    procedure SearchBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure SampleCick(Sender: TObject);
  public
    letters:array of TLetterrec;
    solutioncount:integer;
    savetext:string; {The original sentence}
    procedure Solvefor(btn:TButton; memo:TMemo);
    function countvowels:boolean;
    function CountConsonants:boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

var
  {Values for number words}
  First20:array[1..19] of string =
      ('one','two','three','four','five', 'six','seven','eight','nine','ten',
       'eleven','twelve','thirteen','fourteen', 'fifteen',
       'sixteen','seventeen', 'eighteen','nineteen');
  Decades:array[2..9] of string = ('twenty','thirty','forty','fifty',
              'sixty','seventy', 'eighty', 'ninety');
  NumberWords:array[0..100] of string; {filled in by FormCreate method}
  Vowels:set of Char = ['A','E','I','O','U'];
  Consonants: set of Char;
  

Const
  CRLF=#$D#$A;  {Carriage Return+Linefeed}

{*********** FormCreate ************}
procedure TForm1.FormCreate(Sender: TObject);
var
  i :integer;
  suffix:string;
begin

  Consonants := ['A'..'Z'] - ['A','E','I','O','U'];
  //consonants:=[];
  //for ch:='A' to 'Z' do  if not ansicontainstext('AEIOU',ch) then consonants:=consonants+[ch];
  {fill in the NumberWords array from 'zero'to 'one hundred ninety-nine'}
  for i:=1 to 19 do
  begin
    NumberWords[i]:=uppercase(first20[i]);
    if i+ 100<high(NumberWords) then NumberWords[i+100]:='ONE HUNDRED '+NumberWords[i];
  end;
  for i:=20 to 99 do
  begin
    if (i) mod 10 >0 then suffix:='-'+first20[(i) mod 10] else suffix:='';
    NumberWords[i]:= uppercase(decades[i div 10]) +  uppercase(suffix);
    if i+100<=high(NumberWords)
    then NumberWords[i+100]:= 'ONE HUNDRED '+ NumberWords[i];
  end;
  Numberwords[0]:='ZERO';
  NumberWords[100]:='ONE HUNDRED';
  Reformatmemo(memo1);
end;

{*********** SearchbtnClick ***********}
procedure TForm1.SearchBtnClick(Sender: TObject);
begin  {SearchBtnClick}
  Label3.caption:='Statistics display here';
  Solvefor(searchbtn,memo1);
end;

{************ SolveFor *************}
procedure TForm1.Solvefor(btn:TButton;memo:TMemo);
    var
      checknextcount:int64;
      starttime:TDateTime;
      lettercount,maxaddedcount:integer;

     {----------- Checknext -------------}
    function checknext(nbr:integer):boolean;
    {recursive search function to check required letter counts against actual
    letter counts found.  Letters[0] holds the statistics for vowels if vowels
    counting was specified in the sentence as a search condition}
    var
      i,j,n:integer;
      s:string;
      stop:integer;
      positions:array of TPoint;
      p:TPoint;

      {.......... Lettercount ........}
      function lettercount(n:integer):integer;
      {return nbr of  occurrences of letter[n] in all testwords}
      {For a solution, this number plus the count (InCount) of letter[n] in the
       original message must equal the "value" of the numberword for each tested
       letter.}
      var
        i,j:integer;
        ch:char;
      begin
        ch:= letters[n].letter;
        result:=0;
        if ch {letters[n].letter} <>'0' then {'0' letter marks inactive vowel or consonant records}
        begin
          for i:=0 to high(Letters) do
          with Letters[i] do
          if letter<>'0' then   {again, don't check the inactive records}
          for j:=1 to length(testword) do
          begin
            if (n=0) and countvowels and (testword[j] in vowels) then inc(result)
            else if (n=1) and countconsonants and (testword[j] in consonants) then inc(result)
            else if testword[j]=ch then inc(result);
          end;
        end;
      end; {lettercount}

    begin  {=========== CheckNext ===========}
      result:=false;
      inc(checknextcount);
      if (checknextcount and $FFFF) = 0 then
      begin
        Label3.caption:=format('%.0n number word combinations  tested  ',
           [checknextcount+0.0]);
        application.processmessages;
      end;
      if tag>0 then exit;
      if nbr>high(letters) then
      begin {processed all letters and have a full set of testwords,
            now check if this set of number words forms a solution}
        n:=0;
        for i:=0 to high(Letters) do
        with Letters[i] do
        begin
          n:=lettercount(i);
          testcount:=incount+n;
        end;
        if  (n>maxaddedcount) then
        begin  {ran out of number words}
          result:=false;;
          exit;   {abandon the search}
        end;

        {all the counts have been accumulated, check and see if found values
         = target values}
        result:=true;
        for i:=high(Letters) downto 0 do
        with Letters[i] do
        if letter<>'0' then
        begin
          if testcount<> testvalue then
          begin
            result:=false;
            break;
          end;
        end;
        if result then {it is a solution!}
        begin
          inc(solutioncount);
          s:=savetext;
          s:=stringreplace(s,CRLF,' ',[rfreplaceall]);
          s:=stringreplace(s,'*','',[rfreplaceall]);

          {Plan B}
          {Filling the solution into the original input text using the previous
          technique proved too cumbersome once "Consonant" counting was added.
          Inserted number words for letters could be could appear before, between,
          or after the Vowel and Consonant counts if both were required. Also
          either Vowel or Consonant counts could appear first.  Plan A tried to
          keep track of how much each letter count was shifted by inserting
          Vowel and Consonant counts.  The revise algorithm divorces the
          insertion order from the record order by an array sorting record indexes
          by insertion position.  Processing this list from high to low positions,
          (right to left) allows allow insertion to be made without affect the
          original underscore positions for items left of that point. About
          100 lines of Plan A code were obsolete and removed with Plan B.}


          {Sort an array of points containing position and record index by
          descending letter positions so we can process in order from high to low
          This will alow expanding the solution text with number words}
          setlength(positions, length(letters));
          for i:=0 to high(letters) do positions[i]:=point(letters[i].position,i);

          for i:=0 to high(letters)-1 do
          for j:= i+1 to high(letters) do
          if positions[i].x<positions[j].x then
          begin  {swap to move lower positions after higher}
            p:=positions[i];
            positions[i]:=positions[j];
            positions[j]:=p;
          end;

          for i:=0 to high(positions) do
          {process the descending position list so that length changes will not
           affect the other position values}
          begin
            n:=positions[i].y;
            with letters[n] do
            if (letter<>' ') and (position>0)
            then
            begin
              j:=position;
              if s[j]<>' '  then insert(' ',s,j);
              while s[j]='_' do dec(j);
              delete(s,j,position-j+1);  {delete the underscores}

              Insert(' '+lowercase(testword),s,j); {insert the number word}
            end;
          end;
         s:=stringreplace(s,'_',' ',[rfreplaceall]);
         i:=length(s);
         while i>=2 do
         begin {reduce contiguous space characters to single space}
           if (s[i]=' ') and (s[i-1] = ' ') then delete(s,i-1,1);
           dec(i);
         end;
         with  memo. lines do
         begin
           add('');
           Add('Solution '+inttostr(solutioncount)+':');
           Add('');
           add(s); {display the solution}
           add(stringofchar('*',30));
          end;
          Label3.caption:=format('%.0n number word combinations  tested  ',
           [checknextcount+0.0]);
        end;
        if (Letters[0].testcount>=high(numberWords)) or
           (Letters[1].testcount>=high(numberWords))
           then result:=true; {stop the search}
      end
      else {still checking letters}
      with letters[nbr]  do
      if letter<>'0' then
      begin
        {incount +11*length(numbers) is the highest possiuble valid numberword,
        normally no need to check to 199}
        stop:=incount + 11*length(letters);
        If not countvowels then dec(stop,11);
        If not countconsonants then dec(stop,11);
        if stop>high(NumberWords) then stop:=high(NumberWords);
        if incount>0 then
        begin
          for i:=incount to stop do
          begin  {try all NumberWords for this letter}
            testvalue :=i;
            testword:=uppercase(NumberWords[i]);
            testcount:=0;
           {filled in a guess, now check next letter with a recursive call}
            if nbr<=high(letters) then result:= checknext(nbr+1);
          end;
        end
        else
        begin  {this should neven occur}
          testvalue:=0;
          testword:='ZERO';
          testcount:=0;
        end;
      end
      else if nbr<=high(letters) then result:= checknext(nbr+1);
    end; {checknext}


var
  s:string;
  start:integer;
  i,j,n:integer;
  OK:boolean;
  savecaption:string;

begin  {SolveFor}
  screen.cursor:=crHourGlass;
  if btn.caption='Stop' then
  begin {User want to stop search}
    btn.caption:=savecaption;
    tag:=1;
    exit
  end
  else
  begin {User requested search}
    savecaption:=btn.caption;
    btn.caption:='Stop';
    tag:=0;
  end;

  with memo do
  begin
    i:=0;
    s:='';
    repeat  {make sure that each line ends with a space}
      s:=s+  lines[i]+' ';
      inc(i);
    until (i>lines.count-1) or (pos('********' ,lines[i])>0);
    If i>1 then
    for j:= lines.count-1 downto i do  lines.delete(j);

    s:='';

    for i:= 0 to lines.count-1 do
    if length(lines[i])>0 then s:=s+lines[i];
    S:=Uppercase(S);

    Lettercount:=0; {no need to check for counts > lettercount}
    for i:=1 to length(s) do if s[1] in ['A'..'Z'] then inc(LetterCount);
    lines.add(stringofchar('*',30));
    lines.add('');
    savetext:=memo.Text;
  end;


  setlength(letters,2);
  letters[0].letter:='X'; {initialize to no vowel counting}
  letters[1].letter:='X'; {initialize to no vowel counting}
  start:=1;
  OK:=true;

  {find patterns matching:   letter + appostrophe + s   e.g  T's }
  repeat
    n:=posex('''S',s,start); {find the aspostrophe 's}

    if n>0 then
    begin
      if (n>2) and (s[n-1] in ['A'..'Z']) and (s[n-2]  in [' ','_','''']) then
      begin
        s[n-2] :=' ';
        i:=n-2;  {guarantee at least 1 space character}
        while (i>1) and (s[i]=' ') do dec(i); {backup over extra spaces}
        if s[i]='_' then  {Found the '_'}
        begin
          setlength(letters,length(letters)+1);
          with letters[high(letters)] do
          begin
            letter:=s[n-1];
            position:=i; {positioned at the rightmost '_'}
            InCount:=0;
          end;
        end;
      end;
      start:=N+1;
    end;
  until (n=0);

  n:=pos('VOWELS',s);  {look for vowels word preceeded by underscore character}
  if n>0 then
  begin
    i:=n-1;
    while (i>1) and (s[i]=' ') do  dec(i);
    with letters[0] do
    begin
      if s[i]='_' then
      begin
        letter:='*'; {means check vowels}
        position:=i;
        Incount:=0;
        Testword:='';
        Testvalue:=0;
        Testcount:=0;
      end
      else letter:='0';  {no vowel or consonant check}
    end;
  end
  else
  with letters[0] do
  begin {nullify vowel counting}
    letter:='0';
    incount:=0;
    Testword:='';
    Testvalue:=0;
    Testcount:=0;
  end;

  n:=pos('CONSONANTS',s); {Look for consonants word preceeded by underscore character}
  if n>0 then
  begin
    i:=n-1;
    while (i>1) and (s[i]=' ') do  dec(i);
    with letters[1] do
    begin
      if s[i]='_' then
      begin
        letter:='~'; {means check consonants}
        position:=i;
        Incount:=0;
        Testword:='';
        Testvalue:=0;
        Testcount:=0;
      end
      else  letter:='0';
    end;
  end
  else
  with letters[1] do
  begin
    letter:='0';
    incount:=0;
    Testword:='';
    Testvalue:=0;
    Testcount:=0;
  end;

  {get base count of vowels and letters to be tested}
  for i:=1 to length(s) do
  begin
    for j:=2 to high(letters) do
    with letters[j] do
    if s[i]=letter then
    begin
      inc(incount);
    end;
    with letters[0] do
    if countvowels and  (s[i] in vowels) then
    begin
      inc(Incount);
      if incount>high(NumberWords) then
      begin
        showmessage(format('Text has more than %d vowels, the maximum allowed',
                [high(NumberWords)]));
        OK:=false;
        break;
      end;
    end;
    with letters[1] do
    if countConsonants and  (s[i] in consonants) then
    begin
      inc(Incount);
      if incount>high(NumberWords) then
      begin
        showmessage(format('Text has more than %d consonants, the maximum allowed',
                [high(NumberWords)]));
        OK:=false;
        break;
      end;
    end;
  end;
  {start testing}

  solutioncount:=0;
  maxaddedcount:=11*length(letters);
  if lettercount<maxaddedcount then maxaddedcount:=lettercount;
  checknextcount:=0;
  starttime:=now;
  if OK then checknext(0);
  Label3.caption:=format('%.0n number word combinations were tested  in %.1n seconds.',
           [checknextcount+0.0, (now-starttime)*secsperday]);
  screen.cursor:=crDefault;
  btn.caption:=savecaption;
  movetotop(memo);

  if solutioncount>0
  then showmessage (inttostr(solutioncount) + ' solution(s) found' )
  else showmessage('No solution found');
end; {SolveFor}

{********* Countvowels *********}
function TForm1.countvowels:boolean;
{return true if vowels are to be counted}
begin
  result:=letters[0].letter ='*' ;
end;

{********* CountConsonants *********}
function TForm1.countConsonants:boolean;
{return true if vowels are to be counted}
begin
  result:=letters[1].letter ='~';
end;



{******** Image1Click *************}
procedure TForm1.Image1Click(Sender: TObject);
{try the Mensa Calendar sample sentence }
begin
  with memo1 do
  begin
    Clear;
    lines.add('This hat contains exactly ___ S''s, ___ I''s, and ___ E''s.');
    lines.Add('');
    lines.add('*****************************************');
    Solvefor(Searchbtn,memo1);
  end;
end;

{************** SampleClick **********8}
procedure TForm1.SampleCick(Sender: TObject);
{Copy a sample sentence to the input area in Memo1}
begin
  with memo1 do
  begin
    clear;
    lines.add(Tmemo(sender).text);
    lines.add('*****************************************');
  end;
end;

procedure TForm1.StaticText1Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://www.delphiforfun.org/',
  nil, nil, SW_SHOWNORMAL) ;
end;

end.



