unit oge2_HashList;

interface

uses
  Classes;

type
  TDeleteType = (dtDelete, dtDetach);

  { Class THashList
  }

  THashItem = record
    key: longint;
    obj: TObject;
  end;

  PHashItemList = ^THashItemList;
  THashItemList = array [0 .. 0] of THashItem;

  THashList = class(TObject)
  private
    Flist: PHashItemList;
    Fcount: integer;
    Fcapacity: integer;
    memSize: longint;
    FdeleteType: TDeleteType;

  protected
    procedure Error;
    function Get(Index: integer): THashItem;
    procedure Grow;
    procedure Put(Index: integer; const Item: THashItem);
    procedure SetCapacity(NewCapacity: integer);
    procedure SetCount(NewCount: integer);

  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Item: THashItem): integer;
    procedure Clear(dt: TDeleteType);
    procedure Detach(Index: integer);
    procedure Delete(Index: integer);
    function Expand: THashList;
    function IndexOf(key: longint): integer;
    procedure Pack;

    property DeleteType: TDeleteType read FdeleteType write FdeleteType;
    property Capacity: integer read Fcapacity write SetCapacity;
    property Count: integer read Fcount write SetCount;
    property Items[Index: integer]: THashItem read Get write Put; default;
  end;

  { Class THashTable
    the real hashtable.
  }

  THashTable = class(TObject)
  private
    Ftable: THashList;

    procedure Error;

    function getCount: integer;
    procedure SetCount(Count: integer);
    function getCapacity: integer;
    procedure SetCapacity(Capacity: integer);
    function getItem(index: integer): TObject;
    procedure setItem(index: integer; obj: TObject);
    function getDeleteType: TDeleteType;
    procedure setDeleteType(dt: TDeleteType);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const key: string; value: TObject);
    function Get(const key: string): TObject;
    procedure Detach(const key: string);
    procedure Delete(const key: string);
    procedure Clear(dt: TDeleteType);
    procedure Pack;

    property DeleteType: TDeleteType read getDeleteType write setDeleteType;
    property Count: integer read getCount write SetCount;
    property Capacity: integer read getCapacity write SetCapacity;
    property Items[index: integer]: TObject read getItem write setItem;
    property Table: THashList read Ftable;
  end;

Function Hash(Str: String): longint;
// function hash(key: Pointer; length: longint; level: longint): longint;

implementation

uses
  SysUtils { , Consts } ;

type
  longArray = packed array [0 .. 3] of byte;
  longArrayPtr = ^longArray;

  array12 = packed array [0 .. 11] of byte;
  array12Ptr = ^array12;

  longPtr = ^longint;


  {$ifopt Q+} {$define OverFlowCheck} {$endif} {$Q-}
  {$ifopt R+} {$define RangeCheck} {$endif} {$R-}

  { ----------------------------------------------------------------------------- }

constructor THashList.Create;
begin
  FdeleteType := dtDelete;
  Fcapacity := 0;
  Fcount := 0;
  memSize := 4;
  Flist := AllocMem(memSize);
  SetCapacity(100);
end;

{ ----------------------------------------------------------------------------- }
destructor THashList.Destroy;
begin
  Clear(FdeleteType);
  FreeMem(Flist, memSize);
end;


{ ----------------------------------------------------------------------------- }
function THashList.Add(const Item: THashItem): integer;
begin
  Result := Fcount;
  if (Result = Fcapacity) then
    Grow;

  Flist^[Result].key := Item.key;
  Flist^[Result].obj := Item.obj;
  Inc(Fcount);
end;

{ ----------------------------------------------------------------------------- }
procedure THashList.Clear(dt: TDeleteType);
var
  i: integer;
begin
  if (dt = dtDelete) then
    for i := Fcount - 1 downto 0 do
      if (Items[i].obj <> nil) then
        Items[i].obj.Free;
  { FreeMem(FList, memSize);
    memSize:= 4;
    Flist:= AllocMem(memSize); }
  Fcapacity := 0;
  Fcount := 0;
end;

{ ----------------------------------------------------------------------------- }
{
  Detach remove the item from the list without disposing the object
}
procedure THashList.Detach(Index: integer);
begin
  if ((Index < 0) or (Index >= Fcount)) then
    Error;
  Dec(Fcount);
  if (Index < Fcount) then
    System.Move(Flist^[Index + 1], Flist^[Index],
      (Fcount - Index) * SizeOf(THashItem));
end;

{ ----------------------------------------------------------------------------- }
{
  Delete remove the item from the list AND dispose the object
}
procedure THashList.Delete(Index: integer);
begin
  if ((Index < 0) or (Index >= Fcount)) then
    Error;
  Dec(Fcount);
  if (Index < Fcount) then
  begin
    Flist^[Index].obj.Free;
    System.Move(Flist^[Index + 1], Flist^[Index],
      (Fcount - Index) * SizeOf(THashItem));
  end;
end;

{ ----------------------------------------------------------------------------- }
procedure THashList.Error;
begin
  // raise EListError.CreateRes(SListIndexError);
end;

{ ----------------------------------------------------------------------------- }
function THashList.Expand: THashList;
begin
  if (Fcount = Fcapacity) then
    Grow;
  Result := Self;
end;

{ ----------------------------------------------------------------------------- }
function THashList.Get(Index: integer): THashItem;
begin
  if ((Index < 0) or (Index >= Fcount)) then
    Error;
  Result.key := Flist^[Index].key;
  Result.obj := Flist^[Index].obj;
end;

{ ----------------------------------------------------------------------------- }
procedure THashList.Grow;
var
  Delta: integer;
begin
  if Fcapacity > 8 then
    Delta := 16
  else if Fcapacity > 4 then
    Delta := 8
  else
    Delta := 4;
  SetCapacity(Fcapacity + Delta);
end;

{ ----------------------------------------------------------------------------- }
function THashList.IndexOf(key: longint): integer;
begin
  Result := 0;
  while (Result < Fcount) and (Flist^[Result].key <> key) do
    Inc(Result);
  if Result = Fcount then
    Result := -1;
end;

{ ----------------------------------------------------------------------------- }
procedure THashList.Put(Index: integer; const Item: THashItem);
begin
  if (Index < 0) or (Index >= Fcount) then
    Error;
  Flist^[Index].key := Item.key;
  Flist^[Index].obj := Item.obj;
end;

{ ----------------------------------------------------------------------------- }
procedure THashList.Pack;
var
  i: integer;
begin
  for i := Fcount - 1 downto 0 do
    if Items[i].obj = nil then
      Delete(i);
end;

{ ----------------------------------------------------------------------------- }
procedure THashList.SetCapacity(NewCapacity: integer);
begin
  if ((NewCapacity < Fcount) or (NewCapacity > MaxListSize)) then
    Error;
  if (NewCapacity <> Fcapacity) then
  begin
    { FList:= } ReallocMem(Flist, { memSize, } NewCapacity * SizeOf(THashItem));
    memSize := NewCapacity * SizeOf(THashItem);
    Fcapacity := NewCapacity;
  end;
end;

{ ----------------------------------------------------------------------------- }
procedure THashList.SetCount(NewCount: integer);
begin
  if ((NewCount < 0) or (NewCount > MaxListSize)) then
    Error;
  if (NewCount > Fcapacity) then
    SetCapacity(NewCount);
  if (NewCount > Fcount) then
    FillChar(Flist^[Fcount], (NewCount - Fcount) * SizeOf(THashItem), 0);
  Fcount := NewCount;
end;

{ --- Class THashTable ---
  it's just a list of THashItems.
  you provide a key (string) and an object;
  a unique numeric key (longint) is compute (see hash);
  when you get an object, you provide string key, and as fast as possible
  the object is here.
  Really fast;
  Really smart, because of string keys.
}

{ ----------------------------------------------------------------------------- }
constructor THashTable.Create;
begin
  inherited Create;
  Ftable := THashList.Create;
end;

{ ----------------------------------------------------------------------------- }
destructor THashTable.Destroy;
begin
  Ftable.Free;
  inherited Destroy;
end;

{ ----------------------------------------------------------------------------- }
procedure THashTable.Error;
begin
  // Writeln('ERROR');
  // raise EListError.CreateRes(SListIndexError);
end;

{ ----------------------------------------------------------------------------- }
{
  Add 'value' object with key 'key'
}
procedure THashTable.Add(const key: string; value: TObject);
var
  Item: THashItem;
begin
  Item.key := Hash(key);
  Item.obj := value;
  Ftable.Add(Item);
end;

{ ----------------------------------------------------------------------------- }
{
  Get object with key 'key'
}
function THashTable.Get(const key: string): TObject;
var
  index: integer;
begin
  Result := nil;
  index := Ftable.IndexOf(Hash(key));
  if (index < 0) then
    Error
  else
    Result := Ftable[index].obj;
end;

{ ----------------------------------------------------------------------------- }
{
  Detach (remove item, do not dispose object) object with key 'key'
}
procedure THashTable.Detach(const key: string);
var
  index: integer;
begin
  index := Ftable.IndexOf(Hash(key));
  if (index >= 0) then
    Ftable.Detach(index);
end;

{ ----------------------------------------------------------------------------- }
{
  Delete (remove item, dispose object) object with key 'key'
}
procedure THashTable.Delete(const key: string);
var
  index: integer;
begin
  index := Ftable.IndexOf(Hash(key));
  if (index >= 0) then
    Ftable.Delete(index);
end;

{ ----------------------------------------------------------------------------- }
{
  Clear the list; i.e: remove all the items (detach or delete depending of 'dt')
}
procedure THashTable.Clear(dt: TDeleteType);
begin
  Ftable.Clear(dt);
end;

{ ----------------------------------------------------------------------------- }
procedure THashTable.Pack;
begin
  Ftable.Pack;
end;

{ ----------------------------------------------------------------------------- }
function THashTable.getCount: integer;
begin
  Result := Ftable.Count;
end;

procedure THashTable.SetCount(Count: integer);
begin
  Ftable.Count := Count;
end;

function THashTable.getCapacity: integer;
begin
  Result := Ftable.Capacity;
end;

procedure THashTable.SetCapacity(Capacity: integer);
begin
  Ftable.Capacity := Capacity;
end;

function THashTable.getDeleteType: TDeleteType;
begin
  Result := Ftable.DeleteType;
end;

procedure THashTable.setDeleteType(dt: TDeleteType);
begin
  Ftable.DeleteType := dt;
end;

function THashTable.getItem(index: integer): TObject;
begin
  Result := Ftable[index].obj;
end;

{ ----------------------------------------------------------------------------- }
procedure THashTable.setItem(index: integer; obj: TObject);
var
  Item: THashItem;
begin
  Item.key := Ftable[index].key;
  Item.obj := obj;
  Ftable[index] := Item;
end;

{ ----------------------------------------------------------------------------- }
{ original code from lookup2.c, by Bob Jenkins, December 1996
  http://ourworld.compuserve.com/homepages/bob_jenkins/
  PLEASE, let me know if there is problem with it, or if you have a better one. THANKS.
}
{ function hash(key: Pointer; length: longint; level: longint): longint;
  var
  a,b,c:		longint;
  len:			longint;
  k: 			array12Ptr;
  lp:			longPtr;

  begin
  k:= array12Ptr(key);
  len:= length;
  a:= $9E3779B9;
  b:= a;
  c:= level;

  if((longint(key) and 3) <> 0) then begin
  while(len>=12) do begin	{unaligned }
{ inc(a, (longint(k^[00]) +(longint(k^[01]) shl 8) + (longint(k^[02]) shl 16) + (longint(k^[03]) shl 24)));
  inc(b, (longint(k^[04]) +(longint(k^[05]) shl 8) + (longint(k^[06]) shl 16) + (longint(k^[07]) shl 24)));
  inc(c, (longint(k^[08]) +(longint(k^[09]) shl 8) + (longint(k^[10]) shl 16) + (longint(k^[11]) shl 24)));

  {mix(a,b,c); }
{ inc(a , b xor $FFFFFFFF + 1); inc(a , c xor $FFFFFFFF + 1); a:= a xor (c shr 13);
  inc(b , c xor $FFFFFFFF + 1); inc(b , a xor $FFFFFFFF + 1); b:= b xor (a shl 8);
  inc(c , a xor $FFFFFFFF + 1); inc(c , b xor $FFFFFFFF + 1); c:= c xor (b shr 13);
  inc(a , b xor $FFFFFFFF + 1); inc(a , c xor $FFFFFFFF + 1); a:= a xor (c shr 12);
  inc(b , c xor $FFFFFFFF + 1); inc(b , a xor $FFFFFFFF + 1); b:= b xor (a shl 16);
  inc(c , a xor $FFFFFFFF + 1); inc(c , b xor $FFFFFFFF + 1); c:= c xor (b shr 5);
  inc(a , b xor $FFFFFFFF + 1); inc(a , c xor $FFFFFFFF + 1); a:= a xor (c shr 3);
  inc(b , c xor $FFFFFFFF + 1); inc(b , a xor $FFFFFFFF + 1); b:= b xor (a shl 10);
  inc(c , a xor $FFFFFFFF + 1); inc(c , b xor $FFFFFFFF + 1); c:= c xor (b shr 15);

  inc(longint(k),12);
  dec(len,12);
  end;
  end

  else begin
  while(len>=12) do begin	{aligned }
{ lp:= longPtr(k);
  inc(a, lp^); inc(lp,4);
  inc(b, lp^); inc(lp,4);
  inc(c, lp^);

  {mix(a,b,c); }
{ inc(a , b xor $FFFFFFFF + 1); inc(a , c xor $FFFFFFFF + 1); a:= a xor (c shr 13);
  inc(b , c xor $FFFFFFFF + 1); inc(b , a xor $FFFFFFFF + 1); b:= b xor (a shl 8);
  inc(c , a xor $FFFFFFFF + 1); inc(c , b xor $FFFFFFFF + 1); c:= c xor (b shr 13);
  inc(a , b xor $FFFFFFFF + 1); inc(a , c xor $FFFFFFFF + 1); a:= a xor (c shr 12);
  inc(b , c xor $FFFFFFFF + 1); inc(b , a xor $FFFFFFFF + 1); b:= b xor (a shl 16);
  inc(c , a xor $FFFFFFFF + 1); inc(c , b xor $FFFFFFFF + 1); c:= c xor (b shr 5);
  inc(a , b xor $FFFFFFFF + 1); inc(a , c xor $FFFFFFFF + 1); a:= a xor (c shr 3);
  inc(b , c xor $FFFFFFFF + 1); inc(b , a xor $FFFFFFFF + 1); b:= b xor (a shl 10);
  inc(c , a xor $FFFFFFFF + 1); inc(c , b xor $FFFFFFFF + 1); c:= c xor (b shr 15);

  inc(longint(k),12);
  dec(len,12);
  end;
  end;

  inc(c,length);

  if(len>=11) then inc(c, (longint(k^[10]) shl 24));
  if(len>=10) then inc(c, (longint(k^[9]) shl 16));
  if(len>=9) then inc(c, (longint(k^[8]) shl 8));
  if(len>=8) then inc(b, (longint(k^[7]) shl 24));
  if(len>=7) then inc(b, (longint(k^[6]) shl 16));
  if(len>=6) then inc(b, (longint(k^[5]) shl 8));
  if(len>=5) then inc(b, longint(k^[4]));
  if(len>=4) then inc(a, (longint(k^[3]) shl 24));
  if(len>=3) then inc(a, (longint(k^[2]) shl 16));
  if(len>=2) then inc(a, (longint(k^[1]) shl 8));
  if(len>=1) then inc(a, longint(k^[0]));

  {mix(a,b,c); }
{ inc(a , b xor $FFFFFFFF + 1); inc(a , c xor $FFFFFFFF + 1); a:= a xor (c shr 13);
  inc(b , c xor $FFFFFFFF + 1); inc(b , a xor $FFFFFFFF + 1); b:= b xor (a shl 8);
  inc(c , a xor $FFFFFFFF + 1); inc(c , b xor $FFFFFFFF + 1); c:= c xor (b shr 13);
  inc(a , b xor $FFFFFFFF + 1); inc(a , c xor $FFFFFFFF + 1); a:= a xor (c shr 12);
  inc(b , c xor $FFFFFFFF + 1); inc(b , a xor $FFFFFFFF + 1); b:= b xor (a shl 16);
  inc(c , a xor $FFFFFFFF + 1); inc(c , b xor $FFFFFFFF + 1); c:= c xor (b shr 5);
  inc(a , b xor $FFFFFFFF + 1); inc(a , c xor $FFFFFFFF + 1); a:= a xor (c shr 3);
  inc(b , c xor $FFFFFFFF + 1); inc(b , a xor $FFFFFFFF + 1); b:= b xor (a shl 10);
  inc(c , a xor $FFFFFFFF + 1); inc(c , b xor $FFFFFFFF + 1); c:= c xor (b shr 15);

  result:= longint(c);
  end; }
Function Hash(Str: String): longint;
Var
  i: integer;
Begin


  Result := 1315423911;
  For i := 1 To Length(Str) Do
  Begin
    Result := Result Xor ((Result Shl 5) + Ord(Str[i]) + (Result Shr 2));
  End;
  Result := (Result And $7FFFFFFF);


End;

  {$ifdef RangeCheck} {$R+} {$undef RangeCheck} {$endif}
  {$ifdef OverFlowCheck} {$Q+} {$undef OverFlowCheck} {$endif}

end.
