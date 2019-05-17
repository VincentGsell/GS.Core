{$IFDEF fpc}
 {$MODE delphi}
 {$INLINE ON}
 {$MINENUMSIZE 1}
 {$PACKSET 1}
 {$PACKENUM 1}
{$CODEPAGE UTF8}
{$MACRO ON}
 {$IFOPT D+} {$DEFINE DEBUG} {$ENDIF}
{$ENDIF}

{$IFDEF DCC}
{$ENDIF}

{$IF DEFINED(Win32) or DEFINED(Win64)}
 {$DEFINE WINDOWS}
{$IFEND}


{$IFDEF DEBUG}
  {$rangechecks off}
  {$overflowchecks off}
{$ENDIF}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$scopedenums on}

//Third part cyphers lib.
{$DEFINE LIB_CIPHERS_ENABLED}

//Generic.
// Disable for FPC : We use FPC Stable version (3.04) - And on above version (trunk) - Link error persists when we used .
// Disable for DELPHI too : We fellow FPC for instance to not have 2 codes version to manage.
// TODO : Evaluate eventual Side effect : Is "No generic mode" more speed and/or less memory usage regarding to support and code facilities ?
{.$DEFINE USE_GENERIC}
