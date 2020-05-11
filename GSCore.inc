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

//Third part cyphers lib : Custom cypher system.
{.$DEFINE LIB_CIPHERS_ENABLED}

//Generic.
// Disable for FPC : We use FPC "Stable" - And on above version (3.3/trunk) - Link error persists when we used Generics heavely.
// Disable for DELPHI too : We fellow FPC for instance to not have 2 codes version to manage.
// TODO : Evaluate eventual Side effect : Is "No generic mode" more speed and/or less memory usage regarding to support and code facilities ?
{.$DEFINE USE_GENERIC}

//Service support. Enable this service, it will be automaticaly start.
{.$DEFINE WEBSOCKET_SUPPORT}
{$DEFINE HYPERVISOR_SUPPORT}
{$DEFINE UDP_SERVER_SUPPORT} //Discovery, udp support.

{$IFDEF FPC}
  {$IFDEF CPUARM}
    {$UNDEFINE WEBSOCKET_SUPPORT} //on ARM, FPC Compilation is made, for quality reason, only with "stable" release. mORMot used 3.3+.
  {$ENDIF}
{$ENDIF}

