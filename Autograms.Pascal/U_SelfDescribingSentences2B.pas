unit U_SelfDescribingSentences2B;
{Copyright © 2010, Gary Darby,  www.DelphiForFun.org
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
vowel search, it looks for the word "vowels" preceeded by one
or more spaces preceeded by one or more underscore
characters.
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
    //procedure SearchBtn2Click(Sender: TObject);
    procedure SampleCick(Sender: TObject);
  public
    letters:array of TLetterrec;
    //test:array of testrec;
    solutioncount:integer;
    savetext:string; {The original sentence}
    procedure Solvefor(btn:TButton; memo:TMemo);
    function countvowels:boolean;
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
  Vowels:set of CHAR = ['A','E','I','O','U'];
  maxaddedcount:integer;

{*********** SearchbtnClick ***********}
procedure TForm1.SearchBtnClick(Sender: TObject);
begin  {SearchBtnClick}
  Label3.caption:='Statistics display here';
  Solvefor(searchbtn,memo1);
end;

{************ SolveFor *************}
procedure TForm1.Solvefor(btn:TButton;memo:TMemo);

    var checknextcount:int64;
        starttime:TDateTime;


     {----------- Checknext -------------}
    function checknext(nbr:integer{; testwords:array of testrec}):boolean;
    {recursive search function to check required letter counts against letter counts
     found.  Letters[0] holds the statistics for vowels if vowels counting
     was specified in the sentence as a search condition}
    var
      i,j,n:integer;
      s:string;
      netchange:integer;
      newposition:integer;
      stop:integer;



      {.......... Lettercount ........}
      function lettercount(n:integer):integer;
      {return nbr of  occurrences of letter[n] in all testwords}
      {For a solution, this number plus the count of letter[n] in
       the original message must equal the "value" of the numberword
       for each tested letter. }
      var
        i,j:integer;
        ch:char;
      begin
        ch:= letters[n].letter;
        result:=0;
        if (n=0) and (not countvowels)  then exit;
        for i:=0 to high(Letters) do
        with Letters[i] do
        if (i>0) or ((i=0) and countvowels) then
        begin          {only single letters}
          for j:=1 to length(testword) do if testword[j]=ch then inc(result);
        end;
      end;

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
      begin {processed all letters,
            check if this set of number words forms a solution}
        for i:=0 to high(Letters) do
        with Letters[i] do
        begin
          n:=lettercount(i);
          testcount:=letters[i].incount+n;
          {also count the vowels in this set of number words if requested}
          if countvowels  then
          for j:=1 to length(testword) do
          if (testword[j] in vowels) then inc(Letters[0].testcount);
          if  (n>maxaddedcount) then
          begin  {ran out of number words}
            result:=true;
            exit;   {abandon the search}
          end;
        end;

        {all the counts have been accumulated, check and see if found values
         = target values}
        result:=true;
        for i:=0 to high(Letters) do
        with Letters[i] do
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
          {process the vowels count first since it is always in letters[0] but may
          not be the first in position order. }

          if countvowels then {vowels are to be counted}
          with letters[0] do
          begin
            j:=position;
            while s[j]='_' do dec(j);
            netchange:=length(letters[0].testword) - (position-j);
            delete(s,j,position-j+1);
            Insert(' '+lowercase(letters[0].testword),s,j);
          end
          else netchange:=0;

          for i:=high(letters) downto 1 do
          {process the list backward so that length changes will not affect the
          other position values}
          with letters[i] do
          begin
            {if vowel request was before letter requests in the sentence then
             the position for the letters will be off by the net change in
             length caused by replaceing the underscores with the number word.
             This adjusts for that case}
            if position>letters[0].position then newposition:=position+ netchange
            else newposition:=position;
            j:=newposition;
            while s[j]='_' do dec(j);
            delete(s,j,newposition-j+1);  {delete the underscores}
            Insert(' '+lowercase(Letters[i].testword),s,j); {insert the number word}
          end;
          {remove line breaks}
          stringreplace(s,char($D),'',[rfreplaceall]);
          stringreplace(s,char($A),'',[rfreplaceall]);
          memo.lines.add('');
          memo.Lines.Add('Solution '+inttostr(solutioncount)+':');
          memo.lines.Add('');
          memo.lines.add(s); {display the solution}
          reformatmemo(memo);
          Label3.caption:=format('%.0n number word combinations  tested  ',
           [checknextcount+0.0]);
        end;
        if Letters[0].testcount>=high(numberWords)  then result:=true; {stop the search}
      end
      else {still checking letters}
      with letters[nbr]  do
      begin
        {incount +11*length(numbers) is the highest possiuble valid numberword,
        normally no need to check to 199}
        stop:=incount + 11*length(letters);
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
      end;
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
  begin
    btn.caption:=savecaption;
    tag:=1;

    exit
  end
  else
  begin
    savecaption:=btn.caption;
    btn.caption:='Stop';
    tag:=0;
  end;

  with memo do
  begin
    i:=0;
    s:='';
    repeat
      s:=s+  lines[i]+' ';
      inc(i);
    until (i>lines.count-1) or (pos('********' ,lines[i])>0);

    for j:= lines.count-1 downto i+1 do
    if i>0 then lines.delete(j);
  end;
  s:=uppercase(memo.text);
  savetext:=memo.Text;

  setlength(letters,1);
  with letters[0] do   letter:='X'; {initialize to no vowel counting}
  start:=1;
  OK:=true;

  {find patterns matching 'blank capital letter, appostrophe s'.  e.g ' T's' }
  repeat
    n:=posex('''S',s,start); {find the aspostrophe 's}

    if n>0 then
    begin
      if (n>2) and (s[n-1] in ['A'..'Z']) and (s[n-2]  in [' ','_','''']) then
      begin
        s[n-2] :=' ';
        i:=n-2;
        while (i>1) and (s[i]=' ') do dec(i);
        if s[i]='_' then
        begin
          setlength(letters,length(letters)+1);
          with letters[high(letters)] do
          begin
            letter:=s[n-1];
            position:=i;
            InCount:=0;
          end;
        end;
      end;
      start:=N+1;
    end;

  until (n=0);

  n:=pos('VOWELS',s);
  if n>0 then
  begin
    i:=n-1;
    while (i>1) and (s[i]=' ') do  dec(i);
    if s[i]='_' then
    with letters[0] do
    begin
      letter:='*'; {means check vowels}
      position:=i;
      Incount:=0;
    end
    else letters[0].letter:='0';  {no vowel check}
  end
  else
  with letters[0] do
  begin
    letter:='0';
    incount:=1;  {count of 0 will cause premature exit}
  end;

  {get base count of vowels and letters to be tested}
  for i:=1 to length(s) do
  begin
    for j:=1 to high(letters) do
    with letters[j] do
    if s[i]=letter then
    begin
      inc(incount);
    end;
    with letters[0] do
    begin
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
    end;
  end;
  {start testing}

  solutioncount:=0;
  maxaddedcount:=2*length(letters);
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
  result:=letters[0].letter='*';
end;

{*********** FormCreate ************}
procedure TForm1.FormCreate(Sender: TObject);
var
  i :integer;
  suffix:string;
begin
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

{******** Image1Click *************}
procedure TForm1.Image1Click(Sender: TObject);
{try the Mensa Calendar sample sentence }
var
  i,j:integer;
begin
  with memo1 do
  begin
    i:=0;
    while pos('******',lines[i])=0 do inc(i); {delete the current sentence}
    for j:= i-1 downto 0 do lines.delete(j);
    lines.insert(0,'In this Easter egg shape we have ___ E''s, ___ T''s, and ___ N''s.');
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
