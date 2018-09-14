unit GS.Reference.Persister;

interface

Uses Classes;

Const CST_TEST_NORMAL_ITEM_COUNT      = 100000; //one hundred thousand.
      CST_TEST_BIG_ITEM_COUNT         = 1000000; //one million.
      CST_TEST_HUGE_ITEM_COUNT        = 10000000; //10 million.. Never tested.
      CST_CLUSTER_MEMORY              = 2;

Type
tofContentType = ( ctUnknow,
                   ctString,
                   ctStream,
                   ctInteger,
                   ctUINT32,
                   ctDouble);

Const
  Cst_ContentTypeStr : array[0..5] of string = (
                                                'Unknow',
                                                'String',
                                                'Stream',
                                                'Integer',
                                                'Uint32',
                                                'Double'
                                               );

Type

TOnReferenceInitialLoad = Procedure(Sender : TObject; PercentProgress : Double) of Object;

TofReferencePersister = Class Abstract
private
protected
public
  Procedure WriteGenericData( Key : String;
                                      DataType : tofContentType;
                                      StreamData : TMemoryStream); Virtual; Abstract;
  Procedure ReadGenericDataPrepare( Key : String;
                                      DataType : tofContentType;
                                      StreamData : TMemoryStream); Virtual; Abstract;

  Procedure DeleteGenericData(Key : String); Virtual; Abstract;

  Procedure ReadInfoDataByIndex(aIndex : UInt32; var aDataType : TofcontentType; var aKey : String); Virtual; Abstract;
  Procedure ReadInfoDataByKey(aKey : string; var aDataType : TofcontentType; var Index : UInt64); Virtual; abstract;
  Function IsKeyExists(aKey : String) : Boolean; Virtual; Abstract;

  Procedure Open; Virtual; Abstract;
  Procedure Close; Virtual; Abstract;

  Function EntryCount : UInt64; Virtual; abstract;
  Function Version : String; virtual; abstract;
  Function Description : string; Virtual; abstract;
End;


implementation

end.
