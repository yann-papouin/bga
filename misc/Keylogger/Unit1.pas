unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, ExtCtrls, Spin;

type
  TMainForm = class(TForm)
    Log: TMemo;
    Panel1: TPanel;
    Timer1: TTimer;
    Code: TSpinEdit;
    Start: TButton;
    Dbg: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure StartClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

  tagKBDLLHOOKSTRUCT = packed record
    vkCode: DWORD;
    scanCode: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: PULONG;
  end;

  TKBDLLHookStruct = tagKBDLLHOOKSTRUCT;
  PKBDLLHookStruct = ^TKBDLLHookStruct;

var
  MainForm: TMainForm;
  MainHook: HHOOK;
  Wnd1, Wnd2: array [0 .. 255] of char;

implementation

{$R *.dfm}

uses
  DbugIntf;

 function GetCharFromVirtualKey(Key: Word): string;
 var
    keyboardState: TKeyboardState;
    asciiResult: Integer;
 begin
    GetKeyboardState(keyboardState) ;

    SetLength(Result, 2) ;
    asciiResult := ToAscii(key, MapVirtualKey(key, 0), keyboardState, @Result[1], 0) ;
    case asciiResult of
      0: Result := ' ';
      1: SetLength(Result, 1) ;
      2:;
      else
        Result := ' ';
    end;
 end;

function KeyboardHook(Code: Integer; wParam: wParam; lParam: lParam): Longint;
  stdcall;
var
  hookstruct: PKBDLLHookStruct;
  Line: string;
begin
  Result := 0;

  hookstruct := PKBDLLHookStruct(lParam);

  Line := Format('%s (VK=%.3d [0x%.2x], SC=%.3d [0x%.2x])',
    [GetCharFromVirtualKey(hookstruct.vkCode), Ord(hookstruct.vkCode), Ord(hookstruct.vkCode),
    Ord(hookstruct.scanCode), Ord(hookstruct.scanCode)]);

  MainForm.Log.Lines.Add(Line);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MainHook := SetWindowsHookEx(WH_KEYBOARD_LL, KeyboardHook, hInstance, 0);
  SendDebugFmt('%d', [GetLastError]);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  UnhookWindowsHookEx(MainHook);
end;

procedure TMainForm.StartClick(Sender: TObject);
begin
  Timer1.Enabled := not Timer1.Enabled;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
   Dbg.Lines.Add(Format('%.3x',[Code.Value]));
   Keybd_Event(0, Code.Value, 0, 0);
   Sleep(1000);
   Keybd_Event(0, Code.Value, KEYEVENTF_KEYUP, 0);

 //  Code.Value := Code.Value +1;
end;

end.
