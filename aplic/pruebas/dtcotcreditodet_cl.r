	��VT]Y�9   �                                              .X 398C00EFutf-8 MAIN C:\newsie\on_in_co\aplic\pruebas\dtcotcreditodet_cl.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,AftIgv logical 0 0,AftIsc logical 1 0,CanPed decimal 2 0,CodCia integer 3 0,CodCli character 4 0,CodDiv character 5 0,CodDoc character 6 0,codmat character 7 0,Factor decimal 8 0,FchPed date 9 0,FlgEst character 10 0,Hora character 11 0,ImpDto decimal 12 0,ImpDto2 decimal 13 0,ImpIgv decimal 14 0,ImpIsc decimal 15 0,ImpLin decimal 16 0,Libre_c05 character 17 0,MrgUti decimal 18 0,NroItm integer 19 0,NroPed character 20 0,PorDto decimal 21 0,PorDto2 decimal 22 0,Por_Dsctos1 decimal 23 0,Por_Dsctos2 decimal 24 0,Por_Dsctos3 decimal 25 0,PreBas decimal 26 0,PreUni decimal 27 0,PreVta1 decimal 28 0,PreVta2 decimal 29 0,PreVta3 decimal 30 0,UndVta character 31 0,CanApr decimal 32 0,DesMat character 33 0,DesMar character 34 0,RowNum integer 35 0,RowIdent character 36 0,RowMod character 37 0,RowIdentIdx character 38 0,RowUserProp character 39 0,ChangedFields character 40 0        �S              (@             � �S  0�              ؇              xE     +    � �  W   �� `  X    �   Y   �   [    �   \   8� <  ]   t�    ^   �� 0  `   ? �� �"  iSO8859-1                                                                           \S    �                                      �                   ��                �S  @    t   J   T�              ��  �   �S      �S                                                         PROGRESS                                    
    
                    �              �                                                                                                     
  4         �          �  |*  m   �*     �!  �`�X�+  ;                     `                �   �  �      ,  
    
                    �             �                                                                                          �          
  \  �      �  
    
                  �  �             H                                                                                          �          
    
      �  
    
                  p  8             �                                                                                          
          
  �        0  
    
                    �             �                                                                                                    
  `  *      �  
    
                  �  �             L                                                                                          *          
    <      �  
    
                  t  <  	           �                                                                                          <          
  �  Q      4  
    
                     �  
           �                                                                                          Q          
  d  g      �  
    
                  �  �             P                                                                                          g          
    u      �                         x  @             �                                                                                          u            �  �      8                        $  �             �                                                                                          �            h	  �      �  
    
                  �  �	             T	                                                                                          �          
  
  �      �	  
    
                  |	  D
              
                                                                                          �          
  �
  �      <
  
    
                  (
  �
             �
                                                                                          �          
  l  �      �
                        �
  �             X                                                                                          �              �      �                        �  H                                                                                                       �            �  �      @                        ,  �             �                                                                                          �                �      �                        �  p             \                                                                                          �            �         �       w  X  d>  [   �>  w  5�      ?  (       w             �,          �/      �              �       �  X   R  \   \R  �  *!      �R  )       �         �    �?          C      �                 P�                                               T�          �    L l�                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                                                         	                  
                                                                                                                                                                                                                                                                                                                                                                                         !                  "                  ,                  -                  .                  /                  0                  7                  #                  $                  %                  &                  '                  (                  )                  1                  2                  3                  *                  +                  5                  6                  4                  8                  9                  :                  ;                  <                                 `  h  p  �  x                         �  �  �  �  �          �             �  �  �  �  �          �             �  �  �  �  �                                 (   @   4                           D   L   T   t   d                          x   �   �   �   �                           �   �   �   �   �                          �   �   �   !   !          !             $!  ,!  <!  \!  L!                         `!  h!  p!  �!  |!                         �!  �!  �!  �!  �!                         �!  �!  �!  �!  �!                         �!  "  "  $"  "                         ("  0"  4"  D"  <"                         H"  P"  X"  h"  `"                          l"  t"  |"  �"  �"                          �"  �"  �"  �"  �"                         �"  �"  �"  �"  �"                          #  #  #  0#  $#                         4#  <#  L#  d#  X#                         h#  p#  �#  �#  �#          �#             �#  �#  �#  �#  �#                          �#  �#  �#   $                             $  $  $  ,$   $                          4$  <$  H$  p$  \$                         t$  |$  �$  �$  �$                         �$  �$  �$  �$  �$          �$             �$  �$  �$  %  �$          %             (%  4%  <%  L%  D%                         P%  \%  d%  p%                              t%  |%  �%  �%  �%          �%              �%  �%  �%  &  �%          &             &  $&  4&  \&  H&          `&             t&  |&  �&  �&  �&          �&             �&  �&  �&  �&                             �&  �&   '  '                              '  '  ('  0'                             4'  <'  P'  X'                             \'  h'  p'  |'                              �'  �'  �'  �'                              �'  �'  �'  �'                              �'  �'  �'  �'                              �'  �'   (  (                              (  (  0(  <(                             @(  L(  `(  l(                             p(  |(  �(  �(                             �(  �(  �(  �(                             �(  �(  �(  �(                             �(  �(   )  )                              )  )  ()  4)                              8)  D)  P)  \)                              `)  l)  x)  �)                              �)  �)  �)  �)                              �)  �)  �)  �)                              �)  �)  �)  �)                              �)  *  *   *                             $*  0*  @*  L*                             P*  \*  l*  x*                                                                         CodDoc  x(3)    Codigo  Codigo      CodCia  999 Cia Cia 0   C�digo de compa�ia  Factor  >>,>>9.9999 Factor  Factor  0   Factor  UndVta  x(8)    Unidad  Und     Unidad de movimiento    TipVta  X(1)    Tipo Venta  Tipo venta      codmat  X(6)    Codigo Articulo Codigo Articulo     CodAux  X(12)   Codigo Cliente  Codigo!Cliente      NroPed  X(12)   No. Pedido  Numero!Pedido       CanPed  >,>>>,>>9.9999  Cantidad    Cantidad    0   Cantidad despachada PreUni  >,>>>,>>9.99999 Precio Unitario Precio Unitario 0   PorDto  >>9.99  % Dscto.    % Dscto.    0   PorDto2 >>9.99  % Dscto.    % Dscto.    0   ImpDto  >,>>>,>>9.9999  Importe Descuento   Importe!Descuento   0   ImpLin  ->>,>>>,>>9.99  Importe Importe 0   NroItm  >>9 No.Item No.Item 0   AftIgv  Si/No   I.G.V.  I.G.V.  Si  AftIsc  Si/No   I.S.C.  I.S.C.  No  PreBas  >,>>>,>>9.9999  Precio Base Precio Base 0   PreVta  >>,>>>,>>9.99   Precio Venta    Precio Venta    0   ImpIgv  >,>>>,>>9.9999  Importe Igv Importe Igv 0   ImpIsc  >,>>>,>>9.9999  Importe Isc Importe Isc 0   canate  >,>>>,>>9.9999  Cantidad    Cantidad    0   Cantidad atendida   Hora    X(5)    Hora    Hora    string(TIME,"HH:MM")    FlgEst  X(1)    FlgEst  P   FchPed  99/99/9999  Fecha   Fch.Pedido  today   MrgUti  ->>,>>9.99  Margen de Utilidad  Margen de! Utilidad 0   Pesmat  ->>,>>9.9999    Peso    Peso    0   CodCli  x(8)    Codigo  Codigo      C�digo del Cliente  AlmDes  x(3)    Almac�n Despacho    Almac�n!Despacho        Almac�n despacho    Por_Dsctos  ->>9.99 % Dscto % Dscto 0   Flg_Factor  X(1)    Flg_Factor      CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CanPick >,>>>,>>9.9999  Cantidad Pickeada   Cantidad!Pickeada   0   Cantidad pickeada   CanSol  >,>>>,>>9.9999  Cantidad Solicitada Cantidad!Solicitada 0   Cantidad solicitada CanApr  >,>>>,>>9.9999  Cantidad Aprobada   Cantidad!Aprobada   0   Cantidad aprobada   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   AlmTrf  x(8)    AlmTrf      CanTrf  ->>>,>>>,>>9.9999   CanTrf  0   SdoCot  ->>>,>>>,>>9.9999   SdoCot  0   Libre_c01   x(60)   Libre_c01       Libre_c02   x(60)   Libre_c02       Libre_c03   x(60)   Libre_c03       Libre_c04   x(60)   Libre_c04       Libre_c05   x(60)   Libre_c05       Libre_d01   ->>>,>>>,>>9.99<<<  Libre_d01   0   Libre_d02   ->>>,>>>,>>9.99<<<  Libre_d02   0   Libre_d03   ->>,>>9.99  Libre_d03   0   Libre_d04   ->>,>>9.99  Libre_d04   0   Libre_d05   ->>,>>9.99  Libre_d05   0   Libre_f01   99/99/99    Libre_f01   ?   Libre_f02   99/99/99    Libre_f02   ?   Libre_f03   99/99/9999  Libre_f03   ?   Libre_f04   99/99/9999  Libre_f04   ?   Libre_f05   99/99/9999  Libre_f05   ?   CodMatWeb   x(15)   CodMatWeb       DesMatWeb   x(60)   DesMatWeb       CanPedWeb   >,>>>,>>9.9999  CanPedWeb   0   PreUniWeb   >,>>>,>>9.99999 PreUniWeb   0   ImpLinWeb   ->>,>>>,>>9.99  ImpLinWeb   0   �    B W�  ���<������              �    �   strinP�       �    � 00000        ��        ���             �"        �"        �"        �"                �     i  i  i  i      i  i  i  i      i  i  i      i  i  i  i  i     	 	 	 	 	 	 	 	    ,   3   :   A   O   ]   d   k   r   �   �   �   �   �   �   �   �   �   �   �   �   �   V   �   y   H   �   �   �   �         F  P  Z  d  n  x  �  �  �    "  )  1  8  �  �  �  �  �  �  ?  �  �  �  �                                                                                                                                       	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                                 �6  �6  �6  �6  �6                         �6  �6   7  7  7                         7  7  ,7  D7  87          H7             \7  d7  h7  p7  l7          t7             �7  �7  �7  �7  �7          �7             �7  �7  �7  �7  �7          �7             8  8   8  08  (8                         48  <8  D8  d8  T8                         h8  p8  |8  �8  �8          �8             �8  �8  �8  �8  �8                         �8  �8  �8  �8                             �8  �8  �8  9  �8                         9  $9  49  \9  H9                         `9  h9  x9  �9                             �9  �9  �9  �9  �9                         �9  �9  �9  �9  �9                         �9  �9  :  :  :                         :  $:  ,:  8:                             <:  D:  P:  x:  d:                         |:  �:  �:  �:  �:                         �:  �:  �:  �:  �:                         �:  �:  �:  �:  �:                         �:   ;  ;   ;  ;                         $;  0;  8;  H;  @;                         L;  X;  `;  p;  h;                         t;  �;  �;  �;  �;                         �;  �;  �;  �;  �;                         �;  �;  �;  <  �;                         <  <  $<  D<  4<                         H<  P<  `<  �<  p<                         �<  �<  �<  �<  �<                         �<  �<  �<  �<  �<          �<             �<   =  =  8=  $=          <=             P=  X=  `=  x=  l=          |=             �=  �=  �=  �=  �=                         �=  �=  �=  �=                             �=  �=  �=  �=                               >  >  >  >                             >  (>  0>  <>                             @>  L>  T>  `>                                                                          AftIgv  Si/No   I.G.V.  I.G.V.  Si  AftIsc  Si/No   I.S.C.  I.S.C.  No  CanPed  >,>>>,>>9.9999  Cantidad    Cantidad    0   Cantidad despachada CodCia  999 Cia Cia 0   C�digo de compa�ia  CodCli  x(8)    Codigo  Codigo      C�digo del Cliente  CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CodDoc  x(3)    Codigo  Codigo      codmat  X(14)   Codigo Articulo Codigo Articulo     Factor  >>,>>9.9999 Factor  Factor  0   Factor  FchPed  99/99/9999  Fecha   Fch.Pedido  today   FlgEst  X(1)    FlgEst  P   Hora    X(5)    Hora    Hora    string(TIME,"HH:MM")    ImpDto  >,>>>,>>9.9999  Importe Descuento   Importe!Descuento   0   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   ImpIgv  >,>>>,>>9.9999  Importe Igv Importe Igv 0   ImpIsc  >,>>>,>>9.9999  Importe Isc Importe Isc 0   ImpLin  ->>,>>>,>>9.99  Importe Importe 0   Libre_c05   x(60)   Libre_c05       MrgUti  ->>,>>9.99  Margen de Utilidad  Margen de! Utilidad 0   NroItm  >>9 No.Item No.Item 0   NroPed  X(12)   No. Pedido  Numero!Pedido       PorDto  >>9.99  % Dscto.    % Dscto.    0   PorDto2 >>9.99  % Dscto.    % Dscto.    0   Por_Dsctos1 ->>9.99 % Dscto % Dscto 0   Por_Dsctos2 ->>9.99 % Dscto % Dscto 0   Por_Dsctos3 ->>9.99 % Dscto % Dscto 0   PreBas  >,>>>,>>9.9999  Precio Base Precio Base 0   PreUni  >,>>>,>>9.99999 Precio Unitario Precio Unitario 0   PreVta1 >>,>>>,>>9.99   Precio Venta    Precio Venta    0   PreVta2 >>,>>>,>>9.99   Precio Venta    Precio Venta    0   PreVta3 >>,>>>,>>9.99   Precio Venta    Precio Venta    0   UndVta  x(8)    Unidad  Und     Unidad de movimiento    CanApr  >,>>>,>>9.9999  Cantidad Aprobada   Cantidad!Aprobada   0   Cantidad aprobada   DesMat  X(60)   Descripci�n Descripci�n     Descripci�n del material    DesMar  X(20)   Marca   Marca       RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  : J�  ���)������    00000   �Pstring(TIME,"HH:MM")                                 _!        o!        v!                �     i     i     i    % 	' 	( 	    �   �   d   3   �     ,   O   :   �   �   �   �   )  �   �   �   n  �   �   ]   r   y   !  !!  -!  �   k   9!  A!  I!  A   "  Q!  X!  _!  f!  o!  v!  �!                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                                 <J  DJ  LJ  \J  TJ                         `J  hJ  pJ  �J  xJ                         �J  �J  �J  �J  �J          �J             �J  �J  �J  �J  �J          �J             �J   K  K  K  K          K             0K  8K  @K  XK  LK          `K             �K  �K  �K  �K  �K                         �K  �K  �K  �K  �K                         �K  �K  �K  �K  �K           L             L  L  L  0L  $L                         8L  @L  HL  PL                             TL  \L  dL  tL  lL                         �L  �L  �L  �L  �L                         �L  �L  �L  �L                             �L  �L  M  $M  M                         (M  0M  @M  XM  LM                         \M  dM  tM  �M  |M                         �M  �M  �M  �M                             �M  �M  �M  �M  �M                         �M  �M  �M  N   N                         N  N  N  8N  (N                         <N  DN  LN  dN  XN                         hN  pN  xN  �N  �N                         �N  �N  �N  �N  �N                         �N  �N  �N  �N  �N                         �N  �N  �N  O   O                         O  O  $O  <O  0O                         @O  HO  XO  xO  hO                         |O  �O  �O  �O  �O                         �O  �O  �O  �O  �O                         �O  �O  P  ,P  P                         0P  8P  @P  LP  HP          PP             hP  pP  �P  �P  �P          �P             �P  �P  �P  �P  �P          �P             Q  Q  Q  (Q   Q                         ,Q  4Q  @Q  HQ                             LQ  XQ  `Q  lQ                              pQ  xQ  �Q  �Q                             �Q  �Q  �Q  �Q                             �Q  �Q  �Q  �Q                              �Q  �Q  �Q  �Q                                                                          AftIgv  Si/No   I.G.V.  I.G.V.  Si  AftIsc  Si/No   I.S.C.  I.S.C.  No  CanPed  >,>>>,>>9.9999  Cantidad    Cantidad    0   Cantidad despachada CodCia  999 Cia Cia 0   C�digo de compa�ia  CodCli  x(8)    Codigo  Codigo      C�digo del Cliente  CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CodDoc  x(3)    Codigo  Codigo      codmat  X(14)   Codigo Articulo Codigo Articulo     Factor  >>,>>9.9999 Factor  Factor  0   Factor  FchPed  99/99/9999  Fecha   Fch.Pedido  today   FlgEst  X(1)    FlgEst  P   Hora    X(5)    Hora    Hora    string(TIME,"HH:MM")    ImpDto  >,>>>,>>9.9999  Importe Descuento   Importe!Descuento   0   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   ImpIgv  >,>>>,>>9.9999  Importe Igv Importe Igv 0   ImpIsc  >,>>>,>>9.9999  Importe Isc Importe Isc 0   ImpLin  ->>,>>>,>>9.99  Importe Importe 0   Libre_c05   x(60)   Libre_c05       MrgUti  ->>,>>9.99  Margen de Utilidad  Margen de! Utilidad 0   NroItm  >>9 No.Item No.Item 0   NroPed  X(12)   No. Pedido  Numero!Pedido       PorDto  >>9.99  % Dscto.    % Dscto.    0   PorDto2 >>9.99  % Dscto.    % Dscto.    0   Por_Dsctos1 ->>9.99 % Dscto % Dscto 0   Por_Dsctos2 ->>9.99 % Dscto % Dscto 0   Por_Dsctos3 ->>9.99 % Dscto % Dscto 0   PreBas  >,>>>,>>9.9999  Precio Base Precio Base 0   PreUni  >,>>>,>>9.99999 Precio Unitario Precio Unitario 0   PreVta1 >>,>>>,>>9.99   Precio Venta    Precio Venta    0   PreVta2 >>,>>>,>>9.99   Precio Venta    Precio Venta    0   PreVta3 >>,>>>,>>9.99   Precio Venta    Precio Venta    0   UndVta  x(8)    Unidad  Und     Unidad de movimiento    CanApr  >,>>>,>>9.9999  Cantidad Aprobada   Cantidad!Aprobada   0   Cantidad aprobada   DesMat  X(60)   Descripci�n Descripci�n     Descripci�n del material    DesMar  X(20)   Marca   Marca       RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  : J�  ���*������    00000   �Pstring(TIME,"HH:MM")                                 _!        o!        v!                �     i     i     i    % 	' 	( 	    �   �   d   3   �     ,   O   :   �   �   �   �   )  �   �   �   n  �   �   ]   r   y   !  !!  -!  �   k   9!  A!  I!  A   "  Q!  X!  _!  f!  o!  v!  �!  �!    ��                            ����                            �"    p�                    ��    undefined                                                               �       t�  �   l   ��  ��                    �����               0�d                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                     3   ����       $      H  ���                       8      
                       � ߱        �  �   "   D       �     E          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   <ʫ                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  r  u  L              p�X                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  w  }  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                    �  p              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  �  �  p              4�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  �  �  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  �  �  �	              d�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  �  �  H              , q                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  �  �  T              �q                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  �  �  T              Dq                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  �  �  |              PSB                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  �  �  |              �SB                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  �  �  |              LTB                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              �p                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              0p                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              �p                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              �K�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              @L�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              �L�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              ȖM                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              �_�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              �f                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              X�L                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              P��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              4�L                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              ��L                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              H�L                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              lŬ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              �ɬ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �    l%              ��$                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                      d'              �O                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                    
  h(              D�O                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                      �)              $�O                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                      @+              �k                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                      �,              �k                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                  $  )  �-               ]                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                  +  ,  �/              LfU                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                  .  1  �0              �fU                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                  3  4  �1              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                  6  8  �2              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     O      CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 V      CHARACTER,  canNavigate �3      �3      (4    `      LOGICAL,    closeQuery  4      44      `4   
 l      LOGICAL,    columnProps @4      l4      �4    w      CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 �      CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �      CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �      LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �      LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �      CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �      CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    �      LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    �      CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    �      CHARACTER,  hasForeignKeyChanged    88      d8      �8    �      LOGICAL,    openDataQuery   |8      �8      �8          LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	       LOGICAL,    prepareQuery    9      49      d9    %      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    2      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 ?      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 I      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 S      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    ]      CHARACTER,  assignDBRow                             <  �;      ��                       <              �m^                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                  "  '  L=              �7f                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                  )  *  �>              tn�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                  ,  .  �?               o�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                  0  2   A              �s�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                  4  5  PB              l�^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                  7  8  PC              �^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                  :  ;  PD              TY                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                  =  >  PE              \Y                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                  @  A  PF              �!Y                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  C  E  \G              (��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  G  H  �H              l��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  J  L  �I              ���                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  N  O  �J              0iH                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  Q  R  �K              �iH                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  T  W  �L              tjH                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    ~      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP    �      CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P    �      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q           CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !        CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  $      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  3      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  B      CHARACTER,  getForeignValues    @R      lR      �R  %  Q      CHARACTER,  getQueryPosition    �R      �R      �R  &  b      CHARACTER,  getQuerySort    �R      �R      S  '  s      CHARACTER,  getQueryString  �R      (S      XS  (  �      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /  �      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0         CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1        CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2        LOGICAL,    removeQuerySelection    �W      �W      (X  3  0      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  E      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 S      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  ^      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  m      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  ~      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              8gW                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              �gW                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]              |hW                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              ��W                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              \�W                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                       a              ��W                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                      b              гu                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                      Dc              �u                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  
    Hd              ,�u                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  �      CHARACTER,  getASBound  �d      e      0e  = 
 �      LOGICAL,    getAsDivision   e      <e      le  >  �      CHARACTER,  getASHandle Le      xe      �e  ?  �      HANDLE, getASHasStarted �e      �e      �e  @  �      LOGICAL,    getASInfo   �e      �e      f  A 	 �      CHARACTER,  getASInitializeOnRun    �e       f      Xf  B        LOGICAL,    getASUsePrompt  8f      df      �f  C        LOGICAL,    getServerFileName   tf      �f      �f  D  +      CHARACTER,  getServerOperatingMode  �f      �f      g  E  =      CHARACTER,  runServerProcedure  �f      $g      Xg  F  T      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  g      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  u      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              l �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              �Re                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              �Ze                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              �U                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              ,U                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              (
U                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              �
U                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              ��P                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              �P                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              ��P                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              � Q                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �    �x              �Q                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                    	  |z              �P                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                      �{              xP                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                      �|              k                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                      x~              tr                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                      �              (s                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                       ��              p{                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                  "  $  L�              �?N                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                  &  '  t�              XDN                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 4      LOGICAL,    assignLinkProperty  ؃      �      8�  P  ?      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  R      CHARACTER,  getChildDataKey ��      ̄      ��  R  `      CHARACTER,  getContainerHandle  ܄      �      <�  S  p      HANDLE, getContainerHidden  �      D�      x�  T  �      LOGICAL,    getContainerSource  X�      ��      ��  U  �      HANDLE, getContainerSourceEvents    ��      ��      ��  V  �      CHARACTER,  getContainerType    ܅      �      <�  W  �      CHARACTER,  getDataLinksEnabled �      H�      |�  X  �      LOGICAL,    getDataSource   \�      ��      ��  Y  �      HANDLE, getDataSourceEvents ��      ��      �  Z  �      CHARACTER,  getDataSourceNames  Ԇ       �      4�  [  		      CHARACTER,  getDataTarget   �      @�      p�  \  	      CHARACTER,  getDataTargetEvents P�      |�      ��  ]  *	      CHARACTER,  getDBAware  ��      ��      �  ^ 
 >	      LOGICAL,    getDesignDataObject ȇ      �      (�  _  I	      CHARACTER,  getDynamicObject    �      4�      h�  `  ]	      LOGICAL,    getInstanceProperties   H�      t�      ��  a  n	      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �	      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  �	      CHARACTER,  getObjectHidden �      <�      l�  d  �	      LOGICAL,    getObjectInitialized    L�      x�      ��  e  �	      LOGICAL,    getObjectName   ��      ��      �  f  �	      CHARACTER,  getObjectPage   ̉      ��      (�  g  �	      INTEGER,    getObjectParent �      4�      d�  h  �	      HANDLE, getObjectVersion    D�      l�      ��  i  �	      CHARACTER,  getObjectVersionNumber  ��      ��      �  j  
      CHARACTER,  getParentDataKey    Ċ      ��      $�  k  $
      CHARACTER,  getPassThroughLinks �      0�      d�  l  5
      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  I
      CHARACTER,  getPhysicalVersion  ��      ��      �  n  _
      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  r
      CHARACTER,  getQueryObject  �      4�      d�  p  �
      LOGICAL,    getRunAttribute D�      p�      ��  q  �
      CHARACTER,  getSupportedLinks   ��      ��      ��  r  �
      CHARACTER,  getTranslatableProperties   ��      �      (�  s  �
      CHARACTER,  getUIBMode  �      4�      `�  t 
 �
      CHARACTER,  getUserProperty @�      l�      ��  u  �
      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  �
      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  �
      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x        CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y        CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  $      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  2      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  ?      CHARACTER,  setChildDataKey 4�      `�      ��  }  N      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  ^      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    q      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
       LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �        LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  '      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  8      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  N      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  c      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  u      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �        LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �        LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 .      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  9      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  I      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 U      CHARACTER,INPUT pcName CHARACTER    l�    =  ��  0�      �       4   �����                 @�                      ��                  >  k                  ��C                       >  Ě        ?  \�  ؛      �       4   �����                 �                      ��                  @  j                  0�C                       @  l�  �    W  �  ��      �       4   �����                 ��                      ��                  c  e                  ��C                       c  �         d                                  ,     
                    � ߱        �  $  g  ��  ���                           $  i  @�  ���                       x                         � ߱        x�    o  ��  �      �      4   �����                �                      ��                  p  4	                  h�C                       p  ��  H�  o   s      ,                                 ��  $   t  t�  ���                       �  @         �              � ߱        ��  �   u        Ȟ  �   v  �      ܞ  �   x        �  �   z  x      �  �   |  �      �  �   ~  `      ,�  �     �      @�  �   �        T�  �   �  �      h�  �   �         |�  �   �  |      ��  �   �  �      ��  �   �  t      ��  �   �  �      ̟  �   �  ,      ��  �   �  �      ��  �   �  �      �  �   �  P	      �  �   �  �	      0�  �   �   
      D�  �   �  t
      X�  �   �  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  [	  �	  ��              4{�                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ o	  آ  ���                           O   �	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  ]                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  0
                  ,�W                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    `
  T�  Ц      x      4   ����x                �                      ��                  a
  �
                  �<                       a
  d�  ��  �   c
  �      �  �   d
  T      �  �   e
  �      0�  �   f
  D      D�  �   g
  �      X�  �   h
  �      l�  �   j
  p      ��  �   k
  �      ��  �   l
  X      ��  �   m
  �      ��  �   n
  �      Ч  �   o
  D       �  �   p
  �       ��  �   q
  �       �  �   r
  x!       �  �   s
  �!      4�  �   t
  h"      H�  �   u
  �"      \�  �   v
  `#      p�  �   w
  �#      ��  �   x
  X$      ��  �   y
  �$      ��  �   z
  �$      ��  �   {
  L%      Ԩ  �   |
  �%      �  �   }
  <&      ��  �   ~
  �&      �  �   
  4'      $�  �   �
  �'      8�  �   �
  ,(      L�  �   �
  h(      `�  �   �
  �(      t�  �   �
  X)      ��  �   �
  �)      ��  �   �
  *      ��  �   �
  �*      ĩ  �   �
  �*      ة  �   �
  l+      �  �   �
  �+       �  �   �
  \,      �  �   �
  �,      (�  �   �
  L-      <�  �   �
  �-      P�  �   �
  <.      d�  �   �
  �.      x�  �   �
  4/      ��  �   �
  �/          �   �
  $0      d�      ��  8�      T0      4   ����T0                H�                      ��                    �                  TbW                         ̪  \�  �     �0      p�  �     (1      ��  �     �1      ��  �     2      ��  �     �2      ��  �     3      ԫ  �     |3      �  �     �3      ��  �     t4      �  �     �4      $�  �     l5      8�  �     �5      L�  �     d6      `�  �     �6      t�  �      L7      ��  �   !  �7      ��  �   "  <8      ��  �   #  �8      Ĭ  �   $  ,9      ج  �   %  �9      �  �   &  :       �  �   '  X:      �  �   (  �:      (�  �   )  H;      <�  �   *  �;      P�  �   +  8<          �   ,  �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  L                  PdW                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �      ��  ��      K      4   ����K      /     а     �                          3   ����K             �                      3   ����<K  Զ      ,�  ��  �  XK      4   ����XK  
              ��                      ��             
       n                  D�[                         <�  ̱  �     �K      $�  $    ��  ���                       �K     
                    � ߱        8�  �     L      ��  $     d�  ���                       ,L  @         L              � ߱        L�  $    ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V   !  �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $  =  x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   O  �  ���                                      ̵                      ��                  p                    4�[                       p  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   �  �  ���                        adm-clone-props �  ��              �     W     `                          \  `                     start-super-proc    �  d�  �           �     X                                  �                     l�    %  �   �      �X      4   �����X      /   &  ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $  @  ��  ���                       @Y                         � ߱        ��    P  �  \�  ��  \Y      4   ����\Y                и                      ��                  Q  U                  �[                       Q  �  pY                     �Y                     �Y                         � ߱            $  R  l�  ���                             V  �  T�      �Y      4   �����Y  �Y                         � ߱            $  W  (�  ���                       �Y                         � ߱        ع  $  [  ��  ���                       Ժ    ^  ��  �  \�  �Y      4   �����Y      $  _  0�  ���                       Z                         � ߱            �   |  ,Z      lZ     
                �Z                     8\  @        
 �[              � ߱         �  V   �  p�  ���                        �  �   �  D\      �    B  0�  @�      �\      4   �����\      /   C  l�     |�                          3   �����\            ��                      3   �����\  �\     
                P]                     �^  @        
 `^              � ߱        ��  V   O  ��  ���                        �^     
                h_                     �`  @        
 x`              � ߱        ̼  V   s  <�  ���                        L�    �  �  d�      �`      4   �����`                t�                      ��                  �  �                  �K                       �  ��  �  /   �  ��     ��                          3   �����`            н                      3   �����`      /   �  �     �                          3   ����a            <�                      3   ����8a  ��  /  `  x�         la                      3   ����Ta  initProps   x�  ��              �     Y     �                          �  �   	                                   ̿          t�  \�      ��                 8  Q  ��              �p                    O   ����    e�          O   ����    R�          O   ����    ��      �                       ��          ��  p   C  �v  �      N  t�  d�     �v                                        ��                  D  `                  X�P                       D  ��   �  ��     �v                                        ��                  a  }                  dp                       a  ��  ��  |�     w                                        ��                  ~  �                  4p                       ~  �  �  �     $w                                        ��                  �  �                  p                       �  ��  ��  ��     8w                                        ��                  �  �                  Lp                       �  (�  0�   �     Lw                                        ��                  �  �                  �p                       �  ��  ��  ��     `w                                        ��                  �                    �p                       �  @�  H�  8�     tw                                        ��                    +                  x	p                         ��  ��  ��     �w  	                                      ��             	     ,  H                  H
p                       ,  X�  `�  P�     �w  
                                      ��             
     I  e                  Lp                       I  ��  ��  ��     �w                                        ��                  f  �                  p                       f  p�  x�  h�     �w                                        ��                  �  �                  �p                       �  ��  �  ��     �w                                        ��                  �  �                  �p                       �  ��  ��  ��     �w                                        ��                  �  �                  �p                       �  �  �  �      x                                        ��                  �  �                  h�e                       �  ��  ��  ��     x                                        ��                  �                    8�e                       �  ,�  4�  $�     (x                                        ��                    0                  �e                         ��      ��     <x                                        ��                  1  M                  ؚe                       1  D�      O   P  ��  ��  Px               D�          ,�  8�   , �                                                       �     ��                            ����                            ��  d�  ��  �      ��     Z     L�                      � H�  �                      ��    f  �  ��      \x      4   ����\x                ��                      ��                  g  {                  d�e                       g  �  ��  /   h  ��     ��                          3   ����lx            ��                      3   �����x  h�  /   i  (�     8�                          3   �����x            X�                      3   �����x  ��  /   n  ��     ��                          3   �����x            ��                      3   ���� y      /   t   �     �                          3   ���� y            0�                      3   ����@y  `y     
                �y                     ,{  @        
 �z              � ߱        ��  V   �  @�  ���                        ��  $    ��  ���                       @{                         � ߱        h{     
                �{                     4}  @        
 �|              � ߱        ��  V     (�  ���                        t�  $  &  ��  ���                       @}     
                    � ߱        T}     
                �}                        @        
 �~              � ߱        ��  V   0  �  ���                        \�  $  K  ��  ���                       ,     
                    � ߱        @     
                �                     �  @        
 ̀              � ߱        ��  V   U  ��  ���                        D�  $  o  ��  ���                       $�                         � ߱        L�     
                ȁ                     �  @        
 ؂              � ߱        p�  V   y  ��  ���                        ��  �   �  0�      @�  $  �  ��  ���                       P�     
                    � ߱        d�     
                ��                     0�  @        
 ��              � ߱        l�  V   �  ��  ���                        ��  $  �  ��  ���                       <�     
                    � ߱        ��  �   �  P�      0�  $  �  �  ���                       ��     
                    � ߱        D�  �   �  ��      ��  $    p�  ���                       �                         � ߱              #  ��  ��       �      4   ���� �      /   $  ��     �                          3   ���� �  4�     
   $�                      3   ����@�  d�        T�                      3   ����H�  ��        ��                      3   ����\�            ��                      3   ����x�  pushRowObjUpdTable  ��  ��  �                   [      �                               '"                     pushTableAndValidate    ��  4�  �           |     \     �                          �  D"                     remoteCommit    L�  ��  �           p     ]     �                          �  �"                     serverCommit    ��  �  �           l     ^     �                          �  �"                                     4�          �  ��      ��                  G  T  �              Tq                    O   ����    e�          O   ����    R�          O   ����    ��          O   R  ��  ��  ��    ��                            ����                            $�  P�      ��              _      L�                      
�     �"                     disable_UI  ��  ��                      `      �                               �"  
                    �  �    ����  �       ��       ��  8   ����   ��  8   ����   ��  8   ����   ��  8   ����       8   ����       8   ����       ��  ��      viewObject  ,   ��  �  �      toggleData  ,INPUT plEnabled LOGICAL    ��  <�  T�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ,�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  �  (�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE �  |�  ��      removeAllLinks  ,   l�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  �  �      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  ��  ��      hideObject  ,   ��  ��  ��      exitObject  ,   ��  ��  ��      editInstanceProperties  ,   ��   �  �      displayLinks    ,   ��  $�  4�      createControls  ,   �  H�  X�      changeCursor    ,INPUT pcCursor CHARACTER   8�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    t�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  $�  0�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER �  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE x�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  �  ,�      runServerObject ,INPUT phAppService HANDLE  �  X�  l�      disconnectObject    ,   H�  ��  ��      destroyObject   ,   p�  ��  ��      bindServer  ,   ��  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  �  $�      startFilter ,   �  8�  H�      releaseDBRow    ,   (�  \�  l�      refetchDBRow    ,INPUT phRowObjUpd HANDLE   L�  ��  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE ��  ��  ��      fetchDBRowForUpdate ,   ��  �  �      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  H�  X�      compareDBRow    ,   8�  l�  ��      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   \�  ��   �      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  ,�  8�      updateState ,INPUT pcState CHARACTER    �  d�  x�      updateQueryPosition ,   T�  ��  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    |�  ��  ��      undoTransaction ,   ��  �  �      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  ��  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   ��  �  $�      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER    �  p�  ��      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  `�  ��  �      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  L�  `�      startServerObject   ,   <�  t�  ��      setPropertyList ,INPUT pcProperties CHARACTER   d�  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��  �  �      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  �   �      rowObjectState  ,INPUT pcState CHARACTER     �  L�  \�      retrieveFilter  ,   <�  p�  ��      restartServerObject ,   `�  ��  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   ��  ��  ��      refreshRow  ,   ��  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  4�  D�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  $�  t�  ��      initializeServerObject  ,   d�  ��  ��      initializeObject    ,   ��  ��  ��      home    ,   ��  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��   �  ,�      fetchPrev   ,   �  @�  L�      fetchNext   ,   0�  `�  l�      fetchLast   ,   P�  ��  ��      fetchFirst  ,   p�  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   ��  ��  ��      endClientDataRequest    ,   ��  �  �      destroyServerObject ,   ��  ,�  <�      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    �  ��  ��      dataAvailable   ,INPUT pcRelative CHARACTER x�  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  �  ,�      commitTransaction   ,   �  @�  P�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    0�  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 Y%     adecomm/as-utils.w  
"   
   �    }        �
"     
    �     }        ��   @   %               � 
" 	   
 � %              h �P  \         (          
�                          
�            � _   �
" 	   
 C
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 H�               1� o  
 H� z   � %               o%   o           �     H
"   
 H�           �    1� �   H� z   � %               o%   o           � �   H
"   
 H�           �    1� �  
 H� z   � %               o%   o           � �   H
"   
 H�           l    1� �   H� z   � %               o%   o           �     H
"   
 H�           �    1� �   H� z   � %               o%   o           � �   H
"   
 H�           T    1� �   H� �   � %               o%   o           %               
"   
 � �          �    1� �   � �      
"   
 H�               1�    H� z   � %               o%   o           �    H
"   
 H�           �    1� "   H� z   � %               o%   o           � 1  S H
"   
 H�           �    1� �   H� �   � %               o%   o           %               
"   
 H�           p    1� �   H� �   � %               o%   o           %               
"   
 H�           �    1� �   H� �   � %               o%   o           %              
"   
 � �          h    1� �   � � �     
"   
 H�           �    1� �  
 H� �   � %               o%   o           %               
"   
 H�                1� �   H� z   � %               o%   o           �     H
"   
 � �          �    1� �   � �      
"   
 H�           �    1� �   H� z   � %               o%   o           � �  t H
"   
 � �          D	    1� q  
 � �      
"   
 H�           �	    1� |   H� z   � %               o%   o           � �  � H
"   
 H�           �	    1�    H� z   � %               o%   o           �     H
"   
 H�           h
    1� 1  
 H� <   � %               o%   o           %               
"   
 L�           �
    1� @   L� �   � %               o%   o           %              
"   
 L�           `    1� H   L� z   � %               o%   o           �     L
"   
 L�           �    1� Y   L� z   � %               o%   o           o%   o           
"   
 C�           P    1� i  
 C� z   � %               o%   o           �     L
"   
 L�           �    1� t   L� �  	 � %               o%   o           � �  / C
"   
 � �          8    1� �   � � �  	   
"   
 L�           t    1� �   L� �  	 � o%   o           o%   o           �     L
"   
 � �          �    1� �   � � �  	   
"   
 L�           $    1� �   L� �  	 � o%   o           o%   o           �     L
"   
 � �          �    1�    � � �     
"   
 � �          �    1�    � � �  	   
"   
 � �              1�    � � �  	   
"   
 � �          L    1� +   � � �  	   
"   
 C�           �    1� 9   C� �   � o%   o           o%   o           %              
"   
 � �              1� J   � � �  	   
"   
 � �          @    1� X  
 � � c     
"   
 � �          |    1� k   � � �  	   
"   
 � �          �    1� z   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          0    1� �   � � �  	   
"   
 � �          l    1� �  	 � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 L�                1� �   L� z   � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
 �(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�            �� �     p�               �L
�    %              � 8          � $         �           
�    �      
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1�   
 � z   � %               o%   o           �     
"   
 �           <    1� )  
 � z   � %               o%   o           o%   o           
"   
 L�           �    1� 4   L�    � %               o%   o           o%   o           
"   
 L�           4    1� =   L� �   � %               o%   o           %               
"   
 L�           �    1� L   L� �   � %               o%   o           %               
"   
 Y�           ,    1� Y   Y� z   � %               o%   o           �     L
"   
 C�           �    1� `   C� �   � %               o%   o           %              
"   
 C�               1� r   C� �   � %               o%   o           o%   o           
"   
 C�           �    1� ~   C� z   � %               o%   o           o%   o           
"   
 L�               1� �  	 L� z   � %               o%   o           �     L
"   
 L�           �    1� �   L� z   � %               o%   o           o%   o           
"   
 q�               1� �   q� z   � %               o%   o           o%   o           
"   
 L�           �    1� �   L� �   � %               o%   o           %               
"   
 L�           �    1� �   L� �   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� �  
 � �   � %               o%   o           %              
"   
 �           H    1� �   � z   � %               o%   o           o%   o           
"   
 �           �    1� �   � z   � %               o%   o           �     C
"   
 �           8    1� �   � z   � %               o%   o           o%   o           
"   
 � �          �    1�    � �      
"   
 C�           �    1�    C� z   � %               o%   o           � &  ! q
"   
 L�           d    1� H   L� z   � %               o%   o           �     C
"   
 L�           �    1� U   L� z   � %               o%   o           � h   L
"   
 � �          L    1� w   � � �     
"   
 � �          �    1� �   � �      
"   
 L�           �    1� �   L� z   � %               o%   o           �     L
"   
 � �          8     1� �  
 � �      
"   
 Y�           t     1� �   Y� �   � %               o%   o           o%   o           
"   
 C�           �     1� �   C� �   � %               o%   o           %               
"   
 L�           l!    1� �   L� �   � %               o%   o           %               
"   
 C�           �!    1� �   C� z   � %               o%   o           �     L
"   
 C�           \"    1� �   C� z   � %               o%   o           o%   o           
"   
 �           �"    1� �   � �   � %               o%   o           %              
"   
 L�           T#    1�    L� �   � %               o%   o           %               
"   
 Y�           �#    1�    Y� �   � %               o%   o           %               
"   
 � �          L$    1� +   � �      
"   
 � �          �$    1� 8   � � z     
"   
 C�           �$    1� E   C� <   � %               o%   o           o%   o           
"   
 C�           @%    1� Q   C� z   � %               o%   o           �     L
"   
 C�           �%    1� _   C� z   � %               o%   o           o%   o           
"   
 �           0&    1� m   � �   � o%   o           o%   o           o%   o           
"   
 �           �&    1� �   � �  	 � %               o%   o           o%   o           
"   
 �           ('    1� �   � z   � %               o%   o           o%   o           
"   
 L�           �'    1� �  
 L� <   � %               o%   o           o%   o           
"   
 � �           (    1� �   � � z     
"   
 C�           \(    1� �   C� z   � %               o%   o           � �  4 L
"   
 L�           �(    1�   
 L� �   � %               o%   o           %              
"   
 � �          L)    1�    � �      
"   
 q�           �)    1� $   q� z   � %               o%   o           �     Y
"   
 L�           �)    1� 2   L� �   � %               o%   o           %              
"   
 C�           x*    1� A   C� z   � %               o%   o           �     L
"   
 L�           �*    1� N   L� z   � %               o%   o           �     C
"   
 L�           `+    1� \   L� z   � %               o%   o           �     L
"   
 �           �+    1� h   � �   � %               o%   o           %               
"   
 �           P,    1� w  	 �    � %               o%   o           o%   o           
"   
 Y�           �,    1� �   Y� z   � %               o%   o           � �  	 L
"   
 q�           @-    1� �   q� <   � %               o%   o           %       �       
"   
 L�           �-    1� �   L� z   � %               o%   o           �     q
"   
 L�           0.    1� �   L� �   � o%   o           o%   o           %              
"   
 L�           �.    1� �   L� �   � %               o%   o           %               
"   
 L�           (/    1� �   L� z   � %               o%   o           o%   o           
"   
 �           �/    1� �   � �  	 � %               o%   o           �     
"   
 � �          0    1� �   � � �  	   P �L 
�H T   %              �     }        �GG %              
"   
 Y�           �0    1�   
 Y� z   � %               o%   o           �     Y
"   
 L�           1    1�    L� �   � %               o%   o           %               
"   
 L�           �1    1�   	 L� z   � %               o%   o           �     L
"   
 C�           2    1� '   C� z   � %               o%   o           �     L
"   
 L�           �2    1� 5   L� �   � %               o%   o           %               
"   
 L�           �2    1� E   L� z   � %               o%   o           �     L
"   
 L�           p3    1� X   L� z   � %               o%   o           o%   o           
"   
 �           �3    1� `   � z   � %               o%   o           o%   o           
"   
 L�           h4    1� m   L� �   � %               o%   o           o%   o           
"   
 Y�           �4    1� {   Y� �   � %               o%   o           o%   o           
"   
 L�           `5    1� �   L� �   � %               o%   o           o%   o           
"   
 L�           �5    1� �   L� z   � %               o%   o           o%   o           
"   
 q�           X6    1� �  	 q� �  	 � %               o%   o           �     C
"   
 C�           �6    1� �  
 C� �  	 � %               o%   o           �     q
"   
 �           @7    1� �   � z   � %               o%   o           �     C
"   
 �           �7    1� �   � z   � %               o%   o           o%   o           
"   
 L�           08    1� �   L� z   � %               o%   o           o%   o           
"   
 �           �8    1� �   � z   � %               o%   o           �     L
"   
 L�            9    1� �   L� z   � %               o%   o           �     
"   
 L�           �9    1�    L� �  	 � %               o%   o           o%   o           
"   
 � �          :    1�     � �      
"   
 L�           L:    1� ,   L� z   � %               o%   o           �     q
"   
 L�           �:    1� :   L� z   � %               o%   o           o%   o           
"   
 Y�           <;    1� M   Y� �   � %               o%   o           o%   o           
"   
 L�           �;    1� _  
 L� z   � %               o%   o           �     L
"   
 L�           ,<    1� j   L� z   � %               o%   o           �     L
"   
 C�           �<    1� �   C� �   � %               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 C�           p=    1� �  	 C�    � %               o%   o           o%   o           
"   
 C�           �=    1� �   C�    � %               o%   o           o%   o           
"   
 �           h>    1� �   �    � %               o%   o           o%   o           
"   
 Y�           �>    1� �   Y� �   � %               o%   o           %              
"   
 L�           `?    1� �   L� z   � %               o%   o           � �  M Y
"   
 L�           �?    1� 6   L� �   � %               o%   o           %              
"   
 L�           P@    1� G   L� �   � %               o%   o           %               
"   
 L�           �@    1� [   L� �   � %               o%   o           %               
"   
 q�           HA    1� r   q� �  	 � %               o%   o           � �   L
"   
 �           �A    1� �   � �   � %               o%   o           %               
"   
 �           8B    1� �   � �  	 � %               o%   o           o%   o           
"   
 �           �B    1� �   � �   � o%   o           o%   o           %              
"   
 Y�           0C    1� �   Y� �  	 � o%   o           o%   o           �     Y
"   
 L�           �C    1� �   L�    � o%   o           o%   o           o%   o           
"   
 L�            D    1� �   L�    � o%   o           o%   o           o%   o           
"   
 L�           �D    1� �   L� �  	 � o%   o           o%   o           o%   o           
"   
 L�           E    1�    L�    � o%   o           o%   o           o%   o           
"   
 L�           �E    1�    L� �  	 � o%   o           o%   o           � (   L
"   
 L�           F    1� *   L� �  	 � o%   o           o%   o           � 9   L
"   
 L�           |F    1� E   L� �   � %               o%   o           %               
"   
 L�           �F    1� Y   L� �   � %               o%   o           %               
"   
 � �          tG    1� m   � � �  	   
"   
 C�           �G    1� �   C� �   � %               o%   o           %               
"   
 C�           ,H    1� �   C� z   � %               o%   o           o%   o           
"   
 q�           �H    1� �   q� z   � %               o%   o           o%   o           
"   
 �           $I    1� �   � �   � %               o%   o           o%   o           
"   
 L�           �I    1� �   L� z   � %               o%   o           �     L
"   
 �           J    1� �   � �   � %               o%   o           %               
"   
 L�           �J    1� �  	 L� �   � %               o%   o           %                "    � %     start-super-proc � %     adm2/smart.p έP �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6� �     
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �M    �� �   � P   �        �M    �@    
� @  , 
�       �M    �� �   �p�               �L
�    %              � 8       N    � $         �           
�    �    �
"   
 �p� @  , 
�       O    ��    �p�               �L"  	  , �   � "   L� $   � �     }        �A      |    "  	    � "   %              (<   \ (    |    �     }        �A� &   �A"  
  L    "  	  �"  
  L  < "  	  �"  
  L(    |    �     }        �A� &   �A"  
  L
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   �p�               �L
�    %              � 8      Q    � $         �           
�    �    �
"   
 �p� @  , 
�       R    �� o  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 z
"   
   
"   
   (�  L ( l       �        �R    �� �   � P   �        �R    �@    
� @  , 
�       �R    �� �     p�               �L
�    %              � 8      �R    � $         �           
�    �      
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� �     p�               �L%               
"   
  p� @  , 
�       �T    �� �    p�               �L%               
"   
  p� @  , 
�       U    �� �    p�               �L(        �       �       �       �     }        �A
�H T   %              �     }        �GG %              
"   
 q (   � 
"   
 �    �        �U    �� �   �
"   
   � 8      DV    � $         �           
�    �    �
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6� �     
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � O   
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 q"      �       }        �
"   
 � %              %                "    � %     start-super-proc � %     adm2/appserver.p 9�    � �     
�    �     }        �%               %      Server  - �     }        �    "    L�     � %               %      Client      "    L�     � %      NONE    p�,  8         $     "    L        � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �Z    �� �   � P   �        �Z    �@    
� @  , 
�       �Z    �� �   �p�               �L
�    %              � 8      �Z    � $         �           
�    �    �
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "    L        � �   �
�     "    � %     start-super-proc � %     adm2/dataquery.p �L
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �         ]    �� �   � P   �        ,]    �@    
� @  , 
�       8]    �� �   �p�               �L
�    %              � 8      D]    � $         �    �     
�    �    �
"   
 �p� @  , 
�       T^    �� |   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        8_    �� �   � P   �        D_    �@    
� @  , 
�       P_    �� �   �p�               �L
�    %              � 8      \_    � $         �    �     
�    �    �
"   
 �p� @  , 
�       l`    �� �   �p�               �L%               "    � %     start-super-proc � %     adm2/query.p ͭ%     start-super-proc � %     adm2/queryext.p % 	    initProps �
�    %X M H   FOR EACH ITEM NO-LOCK,       EACH Almmmatg OF ITEM NO-LOCK INDEXED-REPOSITION � �   � �     � �     � �     
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        Hb    �� �   � P   �        Tb    �@    
� @  , 
�       `b    �� �   �p�               �L
�    %              � 8      lb    � $         �           
�    �    �
"   
 �p� @  , 
�       |c    �� �   �p�               �L"    ,     "    L� �    � 
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        @d    �� �   � P   �        Ld    �@    
� @  , 
�       Xd    �� �   �p�               �L
�    %              � 8      dd    � $         �           
�    �    �
"   
 �p� @  , 
�       te    ��   	 �p�               �L"    , %� � �   rowObject.Por_Dsctos1 = ITEM.Por_Dsctos[1]  rowObject.Por_Dsctos2 = ITEM.Por_Dsctos[2]  rowObject.Por_Dsctos3 = ITEM.Por_Dsctos[3]  rowObject.PreVta1 = ITEM.PreVta[1]  rowObject.PreVta2 = ITEM.PreVta[2]  rowObject.PreVta3 = ITEM.PreVta[3]  �    "      � �         %              %                   "      %                  "      "      "     T(        "    z%              "    z� �   � "      �       "    ��    "    z� &   � �       � &   ��    "     � &    S    "      "    �     "    [%                � @    �     t T     P   4       � "      (0       4       J"      �       �     �� �   JT ,  %              T   "    J"    � � �     � &   �� �   JT    �    "    J� &   � "      � &   �"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    �%              �     � � �     4  J     "      
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        $k    �� �   � P   �        0k    �@    
� @  , 
�       <k    �� �   �p�               �L
�    %              � 8      Hk    � $         �           
�    �    �
"   
 �p� @  , 
�       Xl    ��   
 �p�               �L"    ,       "  
  �    � �  � L� �   �       "  	    �    � �  � � � �   L�   � �     � �     � �  � ��   � �     � �   �� �  � L      "  
  J�    �     z� �   �       "  	    �    � �   � � �   z   ,        "    �� �   z�   � �   �� �   z�     �    ,        "      � �     �   � �   z� �   � � �   J�   � �     � �     � �  	  
�H T   %              �     }        �GG %              
"   
 � 
"   
 �
"   
 � 
"   
 � (�  L ( l       �        �n    �� �   � P   �        o    �@    
� @  , 
�       o    �� �   � p�               �L
�    %              � 8      o    � $         �           
�    �      
"   
 �p� @  , 
�       ,p    �� �   �p�               �L"    , 
"   
   p� @  , 
�       �p    �� j     p�               �L"    , 
"   
  p� @  , 
�       �p    �� E    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � �  �   � �         "  	  J�     "    [T    "      "      @ A,    �   � �   � � �     "    �"       T      @   "    � (        "      �     ��       � �   �"    z     "  	   %              D H   @ A,    �   � �   �� �     "    �"    L,    S   "    �� �  � L� �   � %                T      @   "    � (        "      �     ��       � �   �"    L     "  
   %                         "    � � �     "    �           "      � �   �"      
�H T   %              �     }        �GG %              
"   
 J
"   
   
"   
 J
"   
 �(�  L ( l       �        �t    �� �   � P   �        u    �@    
� @  , 
�       u    �� �   Jp�               �L
�    %              � 8      u    � $         �    �     
�    �    � 
"   
 �p� @  , 
�       ,v    �� j   �p�               �L"    , 
"   
   p� @  , 
�       �v    �� E     p�               �L"    , "      %              %              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    � %     start-super-proc � %     adm2/data.p %     start-super-proc � %     adm2/dataext.p %     start-super-proc � %     adm2/dataextcols.p %     start-super-proc � %     adm2/dataextapi.p z
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        �y    �� �   � P   �        �y    �@    
� @  , 
�       �y    �� �   �p�               �L
�    %              � 8      �y    � $         �    �     
�    �    �
"   
 �p� @  , 
�       �z    �� �   �p�               �L%               %$     "pruebas/dtcotcreditodet.i" 
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �{    �� �   � P   �        �{    �@    
� @  , 
�       �{    �� �   �p�               �L
�    %              � 8      �{    � $         �           
�    �    �
"   
 �p� @  , 
�       �|    �� r   �p�               �L"    , 
�     	         �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �}    �� �   � P   �        �}    �@    
� @  , 
�       �}    �� �   �p�               �L
�    %              � 8      �}    � $         �           
�    �    �
"   
 �p� @  , 
�       �~    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�       �    �� �   �p�               �L
�    %              � 8      �    � $         �           
�    �    �
"   
 �p� @  , 
�       ��    �� w  	 �p�               �L
"   
 , 
"   
 �      � �!  	   �        �    �
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        ��    �� �   � P   �        ��    �@    
� @  , 
�       ��    �� �   �p�               �L
�    %              � 8      ��    � $         �           
�    �    �
"   
 �p� @  , 
�       ̂    �� �   �p�               �L"    , 
"   
   �       $�    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        ��    �� �   � P   �        ��    �@    
� @  , 
�       ȃ    �� �   �p�               �L
�    %              � 8      ԃ    � $         �           
�    �    �
"   
 �p� @  , 
�       �    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 J        � �!   �
�    
�             �Gp�,  8         $     
"   
 J        � �!   �
�    �    � �!     
�        "    L�     � %     modifyListProperty  
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � x"     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   l       ��                 }  �  �               H�[                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       �U     
                    � ߱              �  (  �      V      4   ����V                �                      ��                  �  �                  ��Z                       �  8  �  �  �  PV            �  �  `      �V      4   �����V                p                      ��                  �  �                  ��[                       �  �  �  o   �      ,                                 �  �   �  �V      �  �   �  �V      $  $  �  �  ���                        W     
                    � ߱        8  �   �  @W      L  �   �  `W      `  �   �  �W          $   �  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �    �               ��Z                    O   ����    e�          O   ����    R�          O   ����    ��      p                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  �[                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O     ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��                 l  +  �                E                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       ta                         � ߱        �  $  �  8  ���                       �a                         � ߱        �a     
                xb                     �c  @        
 �c              � ߱        �  V   �  d  ���                        �        �      �c      4   �����c  �c     
                pd                     �e  @        
 �e              � ߱            V        ���                          $  7  �  ���                       �e                         � ߱        �  $  8  4  ���                       �f                         � ߱          �      4  8                      ��        0         :  P                  T?k      `g     �     :  `      $  :    ���                       �f                         � ߱        �  $  :  `  ���                       g                         � ߱            4   ����@g  lg                     �g                     �g                     h                     0h                         � ߱        d  $  ;  �  ���                             H  �  �      Ph      4   ����Ph      $  I  �  ���                       xh          �i             � ߱        �  $  S    ���                       �i                         � ߱          �        |                      ��        0         U  Z                  Ek      Dj     8     U  @      $  U  �  ���                       �i                         � ߱        l  $  U  @  ���                       �i                         � ߱            4   ����j      $  W  �  ���                       Xj                         � ߱        �j     
                Tk                     �l  @        
 dl              � ߱        �  V   e  �  ���                        �l       
       
       �l       	       	       m                     Dm                         � ߱        �	  $  �  d  ���                       pm       
       
       �m       	       	       �m                     ,n                         � ߱        �	  $  �  	  ���                       �
  $  P  �	  ���                       �n                         � ߱        �n     
                (o                     xp  @        
 8p          �p  @        
 �p          (q  @        
 �p              � ߱        H  V   \  
  ���                          X      �  (                      ��        0         �  �                  |�o      �q           �  �
      $  �  �  ���                       4q                         � ߱          $  �  �  ���                       dq                         � ߱          4   �����q      4   �����q  �  $  �  T  ���                       ,r                         � ߱        �    �  �        Lr      4   ����Lr                p                      ��                  �  �                   p                       �  �  �r                     �r       	       	           � ߱            $  �  (  ���                             �  �  4       s      4   ���� s                �                      ��                  �  �                  � p                       �  �  �s                     t       
       
           � ߱            $  �  D  ���                       Dt                     xt                         � ߱        �  $  �  �  ���                       �t     
                (u                     xv  @        
 8v          �v  @        
 �v              � ߱            V   �  ,  ���                                    7 �          �    �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �   �   �   �   �   �   �   �       $  4  D  T  d  t  �  \  �  �  �  �  8HXhx��������(8HX      �   �   �   �   �   �   �   �      $  4  D  T  d  t  �   \ �  �  �  �  8HXhx��������(8HX  �                    � �                     �          ��                            ����                                                        �   l       ��                  �  �  �               L�K                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                  �  �  �               x�K                    O   ����    e�          O   ����    R�          O   ����    ��      :"       �              �                  $                  d  /  �  $     4  ��                      3   ������            T                      3   ������      O   �  ��  ��  Ć               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  �    �               ,�K                    O   ����    e�          O   ����    R�          O   ����    ��      Y"       �              �                $                  c"       ,             �          n"                                �  /    t     �  �                      3   ����Ȇ            �                      3   �����     /    �     �  �                      3   ���� �  x                             3   ����$�      $     L  ���                                                   � ߱                  �  �                  3   ����0�      $     �  ���                                                   � ߱        X  $  	  ,  ���                       <�                         � ߱            O     ��  ��  X�               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                    :  �               �G                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  c"                    �          n"                      �              /  7  L     \  ��                      3   ����l�  �        |  �                  3   ������      $   7  �  ���                                                   � ߱                                      3   ������      $   7  @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                  �  �  �               Wq                    O   ����    e�          O   ����    R�          O   ����    ��            �  �   �       ��      4   ������      �   �  Ї    ��                            ����                            TXS appSrvUtils ITEM Detalle pedido credito CodDoc CodCia Factor UndVta TipVta codmat CodAux NroPed CanPed PreUni PorDto PorDto2 ImpDto ImpLin NroItm AftIgv AftIsc PreBas PreVta ImpIgv ImpIsc canate Hora FlgEst FchPed MrgUti Pesmat CodCli AlmDes Por_Dsctos Flg_Factor CodDiv CanPick CanSol CanApr ImpDto2 AlmTrf CanTrf SdoCot Libre_c01 Libre_c02 Libre_c03 Libre_c04 Libre_c05 Libre_d01 Libre_d02 Libre_d03 Libre_d04 Libre_d05 Libre_f01 Libre_f02 Libre_f03 Libre_f04 Libre_f05 CodMatWeb DesMatWeb CanPedWeb PreUniWeb ImpLinWeb .\aplic\pruebas\dtcotcreditodet.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "pruebas/dtcotcreditodet.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server Client NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH ITEM NO-LOCK,       EACH Almmmatg OF ITEM NO-LOCK INDEXED-REPOSITION ,   ITEM Almmmatg  rowObject.Por_Dsctos1 = ITEM.Por_Dsctos[1]  rowObject.Por_Dsctos2 = ITEM.Por_Dsctos[2]  rowObject.Por_Dsctos3 = ITEM.Por_Dsctos[3]  rowObject.PreVta1 = ITEM.PreVta[1]  rowObject.PreVta2 = ITEM.PreVta[2]  rowObject.PreVta3 = ITEM.PreVta[3] ; AftIgv AftIsc CanPed CodCia CodCli CodDiv CodDoc codmat Factor FchPed FlgEst Hora ImpDto ImpDto2 ImpIgv ImpIsc ImpLin Libre_c05 MrgUti NroItm NroPed PorDto PorDto2 Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 PreBas PreUni PreVta1 PreVta2 PreVta3 UndVta CanApr DesMat DesMar AftIgv AftIsc CanPed CodCia CodCli CodDiv CodDoc codmat Factor FchPed FlgEst Hora ImpDto ImpDto2 ImpIgv ImpIsc ImpLin Libre_c05 MrgUti NroItm NroPed PorDto PorDto2 Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 PreBas PreUni PreVta1 PreVta2 PreVta3 UndVta CanApr DesMat DesMar INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 PreVta1 PreVta2 PreVta3 DesMat DesMar RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI llave01 llave02 llave03 llave04 qDataQuery |  �7  �  hE      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
 pcViewColList       ��      |        pcRelative  �  ��      �        pcSdoName       ��      �  �     
 pcSdoName       ��      �        plForwards      ��              pcContext       ��      0        plUpdate    `  ��      T        pcFieldList �  ��      x        pcFieldList     ��      �        pcFieldList �  ��      �        piocContext �  ��      �        piocContext   ��              piocContext 8  ��      ,        piocContext \  ��      P        piocContext �  ��      t        piocContext �  ��      �  �     
 piocContext     ��      �        piocContext     ��      �        pcState     ��               pcContext   0  ��      $        piStartRow  T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow      ��      �  �     
 phRowObjUpd     ��               pcProperties    T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow  ,  ��               pcRowIdent      ��      D        pcRowIdent  t  ��      h        pcRowIdent  �  ��      �        pcRowIdent      ��      �        pcRowIdent  �  ��      �        pcValueList     ��      �        pcValueList 4  ��              pcPropertiesForServer       ��      L        pcPropertiesForServer   �  ��      |        pcFieldList �  ��      �        pcFieldList �  ��      �        pcFieldList     ��      �        pcFieldList   ��              pcWhere     ��      ,        pcWhere     ��      L        pcState     ��      l       
 phRowObjUpd �  ��      �       
 phRowObj    �  ��      �       
 phRowObj    �  ��      �        phRowObj        ��      �        phRowObj        ��       	        pioCancel       ��      D	        pcRelative      ��      h	       
 phFilterContainer       ��      �	       
 phRowObjUpd �	  ��      �	        pcRowIdent      ��      �	        pcRowIdent      ��       
       
 phAppService        ��      (
        pcMode  T
  ��      H
       
 phSource    x
  ��      l
        phSource        ��      �
       
 phSource    �
  ��      �
        pcText  �
  ��      �
        pcText      ��      �
        pcText     ��             
 phObject    D  ��      8       
 phObject        ��      \        phObject        ��      �        pcField     ��      �        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller      ��              phCaller        ��      0        phCaller    \  ��      T        pcMod   |  ��      t        pcMod       ��      �       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    (  ��               pdRow       ��      @        pdRow       ��      `       
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   o	  �	  �	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �               �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable             |     cQueryString    |  �  #   Y   �          �                  initProps   �  �  �      7  8  :  ;  H  I  P  S  U  W  Z  e  �  �  P  \  �  �  �  �  �  �  �  �  �  �  �  �  �  +            h     lRet              �        piTableIndex    �  �  (   Z   T  p      �                  deleteRecordStatic  C  D  `  a  }  ~  �  �  �  �  �  �  �  �      +  ,  H  I  e  f  �  �  �  �  �  �  �  �  �  �      0  1  M  N  P  Q                 !       �  �     [       |      �                  pushRowObjUpdTable  �           �        pcValType                  $       �  `     \       �      H                  pushTableAndValidate    �  �  �  �        �        pcContext   �             $       �        �        pcMessages            �        pcUndoIds     0     ]       l                         remoteCommit        	      \             $       �        t        pcMessages            �        pcUndoIds   �  �     ^       D      �                  serverCommit    7  :  �  0     _                                 getRowObjUpdStatic  R  T  �  t     `               h                  disable_UI  �  �  8  �(       �#      �(                      �  �  �  ;   ITEM    �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                                       (         0         8         @         H         P         X         `         h         p        |         �         �         �         �         �         �         �         �         �         �         �                                             $         ,         8         D         P         \         h         t         |         �         �         �         �         CodDoc  CodCia  Factor  UndVta  codmat  NroPed  CanPed  PreUni  PorDto  ImpDto  ImpLin  NroItm  AftIgv  AftIsc  PreBas  PreVta  ImpIgv  ImpIsc  canate  Hora    FlgEst  FchPed  CodAux  MrgUti  PorDto2 TipVta  Pesmat  CodCli  AlmDes  Por_Dsctos  Flg_Factor  CodDiv  CanPick Libre_c01   Libre_c02   Libre_c03   Libre_c04   Libre_c05   Libre_d01   Libre_d02   Libre_f01   Libre_f02   CanSol  CanApr  ImpDto2 AlmTrf  CanTrf  Libre_d03   Libre_d04   Libre_d05   Libre_f05   Libre_f03   Libre_f04   SdoCot  CodMatWeb   DesMatWeb   CanPedWeb   PreUniWeb   ImpLinWeb      �  �  (   RowObject   �         �         �         �         �         �         �         �         �         �                                             $         ,         4         <         H         P         X         `         h         p         |         �         �         �         �         �         �         �         �         �         �         �         �         �         �                   AftIgv  AftIsc  CanPed  CodCia  CodCli  CodDiv  CodDoc  codmat  Factor  FchPed  FlgEst  Hora    ImpDto  ImpDto2 ImpIgv  ImpIsc  ImpLin  Libre_c05   MrgUti  NroItm  NroPed  PorDto  PorDto2 Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 PreBas  PreUni  PreVta1 PreVta2 PreVta3 UndVta  CanApr  DesMat  DesMar  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp         ,   )   RowObjUpd   "          "         ("         0"         8"         @"         H"         P"         X"         `"         h"         p"         x"         �"         �"         �"         �"         �"         �"         �"         �"         �"         �"         �"         �"         �"         �"          #         #         #         #          #         (#         0#         8#         @#         H#         T#         \#         h#         t#         AftIgv  AftIsc  CanPed  CodCia  CodCli  CodDiv  CodDoc  codmat  Factor  FchPed  FlgEst  Hora    ImpDto  ImpDto2 ImpIgv  ImpIsc  ImpLin  Libre_c05   MrgUti  NroItm  NroPed  PorDto  PorDto2 Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 PreBas  PreUni  PreVta1 PreVta2 PreVta3 UndVta  CanApr  DesMat  DesMar  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   �#          �#  
   appSrvUtils �#       �#     xiRocketIndexLimit  �#        �#  
   gshAstraAppserver   $        $  
   gshSessionManager   @$        0$  
   gshRIManager    h$        T$  
   gshSecurityManager  �$        |$  
   gshProfileManager   �$  	 	     �$  
   gshRepositoryManager    �$  
 
     �$  
   gshTranslationManager   %        �$  
   gshWebManager   0%         %     gscSessionId    T%        D%     gsdSessionObj   x%        h%  
   gshFinManager   �%        �%  
   gshGenManager   �%        �%  
   gshAgnManager   �%        �%     gsdTempUniqueID &        �%     gsdUserObj  ,&        &     gsdRenderTypeObj    T&        @&     gsdSessionScopeObj  p&       h&  
   ghProp  �&       �&  
   ghADMProps  �&       �&  
   ghADMPropsBuf   �&       �&     glADMLoadFromRepos  �&       �&     glADMOk '       '  
   ghContainer 8'    	   ,'     cObjectName T'    
   L'     iStart  t'       h'     cAppService �'       �'     cASDivision �'       �'     cServerOperatingMode    �'       �'     cContainerType  (       �'     cQueryString    ((       (  
   hRowObject  H(       <(  
   hDataQuery  h(       \(     cColumns             |(     cDataFieldDefs  �(    �  �(  ITEM    �(    X  �(  RowObject         X  �(  RowObjUpd          "   E   �   �   �   �   =  >  ?  @  W  c  d  e  g  i  j  k  o  p  s  t  u  v  x  z  |  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  4	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  0
  `
  a
  c
  d
  e
  f
  g
  h
  j
  k
  l
  m
  n
  o
  p
  q
  r
  s
  t
  u
  v
  w
  x
  y
  z
  {
  |
  }
  ~
  
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
                                     !  "  #  $  %  &  '  (  )  *  +  ,  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  L                    !  =  O  n  p  �    %  &  @  P  Q  R  U  V  W  [  ^  _  |  �  �  B  C  O  s  �  �  �  �  �  `  f  g  h  i  n  t  {  �      &  0  K  U  o  y  �  �  �  �  �  �  �    #  $      �  .\aplic\pruebas\dtcotcreditodet.w    -  ��  C:\Progress\OpenEdge\src\adm2\data.i 8-  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    h-  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i �-  �W , .\aplic\pruebas\dtcotcreditodet.i    �-  �:   C:\Progress\OpenEdge\src\adm2\query.i    .  z + C:\Progress\OpenEdge\src\adm2\delrecst.i @.  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  t.   ) %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   �.  � ! C:\Progress\OpenEdge\src\adm2\dataquery.i    �.  �Z ( %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i    /  �< " C:\Progress\OpenEdge\src\adm2\appserver.i    d/  �� ' %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �/  I� # C:\Progress\OpenEdge\src\adm2\smart.i    �/  Ds & C:\Progress\OpenEdge\gui\fn  0  tw % %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   <0  Q. $ C:\Progress\OpenEdge\gui\set |0  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i �0  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    �0  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    1  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  `1  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i �1  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i �1   
 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    2  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   P2  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   �2  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �2  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    3  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    X3  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �3  �j  C:\Progress\OpenEdge\gui\get �3  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �3  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    <4  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �4  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �4  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �4  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   (5  �  C:\Progress\OpenEdge\src\adm2\appsprto.i l5  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �5  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �5  ª  %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   $6  �� 	 C:\Progress\OpenEdge\src\adm2\qryprto.i  l6  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   �6  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �6  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    7  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   \7  �    !C:\newsie\on_in_co\aplic\pruebas\dtcotcreditodet_cl.w           �      �7  �   \     �7  [       �7     
  &   8  �   �     8     ,  .   (8  �   "     88          H8  �         X8     �  $   h8  �   �     x8     �  $   �8  �   �     �8     �  $   �8  �   �     �8     q  $   �8  �   o     �8     M  $   �8  �   J     �8     (  $   9  �   &     9       $   (9  �        89     �  $   H9  �   �     X9     �  -   h9  �   �     x9     �  ,   �9  k   Y     �9  �  M      �9     3  +   �9  �  0      �9       +   �9  �        �9     �  +   �9  �  �      :     �  +   :  �  �      (:     �  +   8:  �  �      H:     �  +   X:  �  �      h:     �  +   x:  �  �      �:     h  +   �:  �  e      �:     K  +   �:  �  H      �:     .  +   �:  �  +      �:       +   �:  �        ;     �  +   ;  �  �      (;     �  +   8;  �  �      H;     �  +   X;  �  �      h;     �  +   x;  �  �      �;     �  +   �;  �  }      �;     c  +   �;  �  `      �;     F  +   �;  �  &      �;       $   �;  �        <     �  $   <  k  �      (<     �  $   8<  j  �      H<     w  $   X<  i  v      h<     T  $   x<  _  J      �<     $  *   �<  ^  #      �<     �  *   �<  ]  �      �<     �  *   �<  \  �      �<     �  *   �<  [  �      =     �  *   =  Z  �      (=     a  *   8=  Y  `      H=     :  *   X=  X  9      h=       *   x=  W        �=     �  *   �=  V  �      �=     �  *   �=  U  �      �=     �  *   �=  T  �      �=     w  *   �=  S  v      >     P  *   >  R  O      (>     )  *   8>  Q  (      H>       *   X>  P        h>     �  *   x>  O  �      �>     �  *   �>  N  �      �>     �  *   �>  @        �>     ]  $   �>    ,      �>     
  $   �>          ?     �  $   ?  �   \      (?       )   8?  g   �      H?  a   �  !   X?     �  (   h?  _   �  !   x?     k  $   �?  ]   i  !   �?     G  $   �?  I   3  !   �?  �   *  "   �?     �  '   �?  �   �  "   �?     �  $   �?  �   �  "   @     �  $   @  �   �  "   (@     d  $   8@  g   J  "   H@     +     X@  O     "   h@  �   �  #   x@     �  &   �@  �   k  #   �@       %   �@  �     #   �@     �  $   �@  �   �  #   �@     �  $   �@  �   �  #   �@     �  $   A  �   �  #   A     }  $   (A  �   i  #   8A     G  $   HA  }   ;  #   XA       $   hA     �  #   xA     O  "   �A       !   �A     �      �A     U     �A  �   L     �A  O   >     �A     -     �A     �     �A  �   �     B  �   �     B  O   �     (B     ~     8B     0     HB  y        XB  �   �
  
   hB  G   �
     xB     �
     �B     �
     �B  c   8
  
   �B  x   0
     �B  M   
     �B     

     �B     �	     �B  a   �	     �B  �  �	     C     g	     C  �  4	     (C  O   &	     8C     	     HC     �     XC  �   �     hC     �     xC          �C  x        �C     �     �C     �     �C     ~     �C     j     �C     Q     �C  Q   A     �C     �     D     �     D     �     (D     �     8D  ]   {  
   HD     q     XD     )  
   hD          xD       
   �D  Z   �     �D       	   �D     �     �D     �     �D     �     �D  c   �     �D     c     �D          E          E     �      (E     �      8E     &      HE           XE           