(*
  http://www.delphi3000.com/articles/article_3024.asp?SK=DLL
*)

unit MiniLZO;

interface

uses
  Windows;

// "C" routines needed by the linked LZO OBJ filefunction _memcmp (s1,s2: Pointer; numBytes: LongWord): integer; cdecl;procedure _memcpy (s1, s2: Pointer; n: Integer); cdecl;procedure _memmove(dstP, srcP: pointer; numBytes: LongWord); cdecl;procedure _memset (s: Pointer; c: Byte; n: Integer); cdecl;

{$LINK 'minilzo.obj'}
function _lzo1x_1_compress(const Source: Pointer; SourceLength: LongWord; Dest: Pointer; var DestLength: LongWord; WorkMem: Pointer): integer; stdcall; external;function _lzo1x_decompress(const Source: Pointer; SourceLength: LongWord; Dest: Pointer; var DestLength: LongWord; WorkMem: Pointer (* NOT USED! *)): Integer; stdcall; external;function _lzo1x_decompress_safe(const Source: Pointer; SourceLength: LongWord; Dest: Pointer; var DestLength: LongWord; WorkMem: Pointer (* NOT USED! *)): Integer; stdcall; external;function _lzo_adler32(Adler: LongWord; const Buf: Pointer; Len: LongWord): LongWord; stdcall; external;function _lzo_version: word; stdcall; external;function _lzo_version_string: PChar; stdcall; external;function _lzo_version_date: PChar; stdcall; external;

(*
Error codes for the compression/decompression functions. Negative
values are errors, positive values will be used for special but
normal events.
*)

const

LZO_E_OK                 = 0;
LZO_E_ERROR              = -1;
LZO_E_OUT_OF_MEMORY      = -2;    (* not used right now *)
LZO_E_NOT_COMPRESSIBLE   = -3;    (* not used right now *)
LZO_E_INPUT_OVERRUN      = -4;
LZO_E_OUTPUT_OVERRUN     = -5;
LZO_E_LOOKBEHIND_OVERRUN = -6;
LZO_E_EOF_NOT_FOUND      = -7;
LZO_E_INPUT_NOT_CONSUMED = -8;


implementation


procedure _memset(s: Pointer; c: Byte; n: Integer); cdecl;begin  FillChar(s^, n, c);end;procedure _memcpy(s1, s2: Pointer; n: Integer); cdecl;begin  Move(s2^, s1^, n);end;function _memcmp (s1, s2: Pointer; numBytes: LongWord): integer; cdecl;var  i: integer;  p1, p2: ^byte;begin  p1 := s1;  p2 := s2;  for i := 0 to numBytes -1 do  begin    if p1^ <> p2^ then    begin      if p1^ < p2^ then        Result := -1      else        Result := 1;      exit;    end;    inc(p1);    inc(p2);  end;  Result := 0;end;procedure _memmove(dstP, srcP: pointer; numBytes: LongWord); cdecl;begin  Move(srcP^, dstP^, numBytes);  FreeMem(srcP, numBytes);end;


end.
