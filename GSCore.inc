{$IFDEF fpc}
 {$mode delphi}
 {$modeswitch ADVANCEDRECORDS}
 {$INLINE ON}
 {$MINENUMSIZE 1}
 {$PACKSET 1}
 {$PACKENUM 1}
 {$H+}
 {$MACRO ON}
 {$IFOPT D+} {$DEFINE DEBUG} {$ENDIF}
 {$CODEPAGE UTF8}
{$ENDIF}

{$IFDEF DCC}
{$extendedsyntax on}
{$writeableconst on}

{$IF DECLARED(FireMonkeyVersion)}
  {$DEFINE HAS_FMX}
{$ELSE}
  {$DEFINE HAS_VCL}
{$IFEND}

{$ENDIF}

{$IF DEFINED(Win32) or DEFINED(Win64)}
 {$DEFINE WINDOWS}
{$IFEND}


{$IFDEF DEBUG}
  {$rangechecks off}
  {$overflowchecks off}
{$ENDIF}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$scopedenums on}

{$IFDEF FPC}
  {$IFDEF CPUARM}
    {$UNDEFINE WEBSOCKET_SUPPORT} //on ARM, FPC Compilation is made, for quality reason, only with "stable" release. mORMot used 3.3+.
  {$ENDIF}
{$ENDIF}

