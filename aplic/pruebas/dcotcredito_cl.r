	��V��MY�9   �                                              6 39B000EFutf-8 MAIN C:\newsie\on_in_co\aplic\pruebas\dcotcredito_cl.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,Atencion character 0 0,Cmpbnte character 1 0,CodCia integer 2 0,CodCli character 3 0,CodDiv character 4 0,CodDoc character 5 0,CodMon integer 6 0,CodPos character 7 0,CodVen character 8 0,DirCli character 9 0,FchEnt date 10 0,FchPed date 11 0,fchven date 12 0,FlgEst character 13 0,FlgIgv logical 14 0,FmaPgo character 15 0,Glosa character 16 0,LugEnt character 17 0,NomCli character 18 0,NroPed character 19 0,ordcmp character 20 0,RucCli character 21 0,Sede character 22 0,TpoCmb decimal 23 0,usuario character 24 0,LugEnt2 character 25 0,NroCard character 26 0,Libre_c04 character 27 0,Libre_d01 decimal 28 0,CodRef character 29 0,NroRef character 30 0,Libre_c01 character 31 0,FaxCli character 32 0,Libre_c02 character 33 0,PorIgv decimal 34 0,Libre_c05 character 35 0,RowNum integer 36 0,RowIdent character 37 0,RowMod character 38 0,RowIdentIdx character 39 0,RowUserProp character 40 0,ChangedFields character 41 0     @;              P'             �� @;  �              �              xA     +   h� �  W   � `  X   h� d  Y   ��   [   ��   \   �� <  ]   0�    ^   P� 0  `   ? ��    iSO8859-1                                                                           �:   & �                                      �                   ��                �:  �    �  
 �C   T�              ��  �   ;      ;                                                         PROGRESS                         |           
    
                    �              �                                                                                                     
                                                                                                            ��         �             �                                                                                             ��                      �                                                                                          #   ��         �             x                                                                                          ,   ��                      �                                                                                          5   ��         �             p                                                                                          >   ��                       �                                                                                          G   ��         �  	           h                                                                                          O   ��         (  -      �  
    
                  �  X                                                                                                       -          
  �  ?      P  
    
                  <               �                                                                                          ?          
  �  Q      �  
    
                  �  �             l                                                                                          Q          
  ,  ^      �  
    
                  �  \                                                                                                       ^          
  �  q      T  
    
                  @  	             �                                                                                          q          
  �	  �       	  
    
                  �  �	             p	                                                                                          �          
  0
  �      �	  
    
                  �	  `
             
                                                                                          �          
  �
  �      X
  
    
                  D
               �
                                                                                          �          
  �  �                               �
  �             t                                                                                          �            4  �      �                        �  d                                                                                                        �            �  �      \  
    
                  H               �                                                                                          �          
  �  �        
    
                  �  �             x                                                                                          �          
  8  �      �  
    
                  �  h             $                                                                                          �          
  �        `                        L               �                                                                                                      �                                �  �             |                                                                                                      <        �                        �  l             (                                                                                                          -      d                        P  �             �                                                                                          -            P  !       �       �  X  �%  L   &  �  �      `&  )       �             �                 �       "       �       �  X  P9  M   �9  �  �A      �9  *       �         �    '          `*      �                 P�                                             
  T�          @  �  L l                 �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                                 X  d  l  �  x                         �  �  �  �  �                         �  �  �  �  �          �             �  �                              (  0  8  P  D          X             x  �  �  �  �                         �  �  �  �  �          �             �  �  �  �                             �  �  �                                      (   @   4           D              \   d   p   �                              �   �   �   �   �                          �   �   �   �   �                          �   �   !  !  !                         !   !  (!  8!  0!                         <!  D!  L!  t!  `!                         x!  �!  �!  �!  �!                         �!  �!  �!  �!  �!                         �!  �!  �!  �!  �!          �!             "  "   "  <"  ,"                         @"  H"  P"  p"  `"                         t"  |"  �"  �"  �"          �"             �"  �"  �"  �"                             �"  �"  �"  #  �"          #             #  $#  ,#  <#  4#                         @#  H#  P#  x#  d#                         |#  �#  �#  �#  �#                         �#  �#  �#  �#                             �#  �#  �#  �#                             �#  �#  $  $                             $  $  $$  H$  4$                         L$  X$  `$  l$                             p$  x$  �$  �$  �$          �$             �$  �$  �$  �$                             �$  �$  �$  �$  �$                         �$  �$   %  %                             %  %  $%  ,%                             0%  <%  D%  P%                              T%  \%  d%  l%                             p%  |%  �%  �%                             �%  �%  �%  �%                                                                          Atencion    X(30)   Atenci�n    Atenci�n        Cmpbnte X(3)    Tipo Comprobante    Tipo Comprobante    FAC CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodCli  x(11)   Codigo  Codigo      C�digo del Cliente  CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CodDoc  x(3)    Codigo  Codigo      CodMon  9   Moneda  Cod!mon 1   C�digo de moneda    CodPos  x(3)    Postal      CodVen  x(10)   Vendedor    Vendedor        DirCli  x(60)   Direcci�n   Direcci�n       Direcci�n del Cliente   FchEnt  99/99/9999  Fecha Entrega   TODAY   FchPed  99/99/9999  Fecha   Fch.Pedido  today   fchven  99/99/99    Fecha Vencimiento   Fecha!Vcmto.    ?   FlgEst  X(1)    Estado  Estado  P   FlgIgv  Si/no   Con IGV Con IGV Si  FmaPgo  X(8)    Condicion de ventas Condicion de!venta      Glosa   X(50)   Glosa   Glosa       LugEnt  x(60)   Lugar de entrega    Lugar de entrega        NomCli  x(50)   Nombre  Nombre      Nombre del Cliente  NroPed  X(12)   No. Pedido  Numero!Pedido       ordcmp  X(12)   Orden de Compra Orden ! Compra      RucCli  x(11)   Ruc Ruc     Registro Unico de Contribuyente (Cliente)   Sede    x(5)    Sede        TpoCmb  Z,ZZ9.9999  Tipo de cambio  T/Cambio    0   Tipo de cambio  usuario x(10)   usuario usuario     LugEnt2 x(60)   Lugar de entrega    Lugar de entrega        NroCard x(8)    NroCard Nrocard     Libre_c04   x(60)   Libre_c04       Libre_d01   ->>>,>>>,>>9.99<<<  Libre_d01   0   CodRef  x(3)    Referencia      NroRef  X(12)   No. Referencia  Numero!Referencia       Libre_c01   x(60)   Libre_c01       FaxCli  x(13)   Fax Fax     Fax del Cliente Libre_c02   x(60)   Libre_c02       PorIgv  ->>9.99 % I.G.V.    % I.G.V.    0   Libre_c05   x(60)   Libre_c05       RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ) :�  ���*������ FAC  00000    ���P                              �        �        �                �     i     i     i    & 	( 	) 	    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �             '  .  5  :  A  I  Q  Y  c  m  t  {  �  �  �  �  �  �  �  �  �                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                                 �1  �1  �1  �1  �1                         �1  �1  2  ,2  2                         02  82  <2  D2  @2          H2             \2  d2  l2  |2  t2          �2             �2  �2  �2  �2  �2          �2             �2  �2  �2  3  �2                         3  3  3  $3  3          (3             <3  D3  L3  T3                             X3  `3  h3  �3  t3                         �3  �3  �3  �3  �3          �3             �3  �3  �3  �3                             �3  �3  4  4  4                         $4  ,4  84  \4  L4                         `4  h4  p4  �4  x4                         �4  �4  �4  �4  �4                         �4  �4  �4  �4  �4                         �4  �4  �4  5  �4                         5  5  5  @5  ,5                         D5  L5  T5  d5  \5          h5             |5  �5  �5  �5  �5                         �5  �5  �5  �5  �5                         �5  �5  �5  �5  �5          �5             (6  06  86  @6                             D6  L6  X6  t6  h6          x6             �6  �6  �6  �6  �6                         �6  �6  �6  �6  �6                         �6  �6  �6  7   7                         7  7   7  ,7                             07  <7  P7  \7                             `7  h7  p7  |7                             �7  �7  �7  �7  �7                         �7  �7  �7  �7                             �7  �7  �7  �7  �7          �7             8  8  8  (8                             ,8  48  <8  T8  H8                         X8  d8  l8  x8                             |8  �8  �8  �8                             �8  �8  �8  �8                              �8  �8  �8  �8                             �8  �8  �8  �8                              9  9  9   9                              $9  49  <9  L9                                                                          Atencion    X(30)   Atenci�n    Atenci�n        Cmpbnte X(3)    Tipo Comprobante    Tipo Comprobante    FAC CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodCli  x(11)   Codigo  Codigo      C�digo del Cliente  CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CodDoc  x(3)    Codigo  Codigo      CodMon  9   Moneda  Cod!mon 1   C�digo de moneda    CodPos  x(3)    Postal      CodVen  x(10)   Vendedor    Vendedor        DirCli  x(60)   Direcci�n   Direcci�n       Direcci�n del Cliente   FchEnt  99/99/9999  Fecha Entrega   TODAY   FchPed  99/99/9999  Fecha   Fch.Pedido  today   fchven  99/99/99    Fecha Vencimiento   Fecha!Vcmto.    ?   FlgEst  X(1)    Estado  Estado  P   FlgIgv  Si/no   Con IGV Con IGV Si  FmaPgo  X(8)    Condicion de ventas Condicion de!venta      Glosa   X(50)   Glosa   Glosa       LugEnt  x(60)   Lugar de entrega    Lugar de entrega        NomCli  x(50)   Nombre  Nombre      Nombre del Cliente  NroPed  X(12)   No. Pedido  Numero!Pedido       ordcmp  X(12)   Orden de Compra Orden ! Compra      RucCli  x(11)   Ruc Ruc     Registro Unico de Contribuyente (Cliente)   Sede    x(5)    Sede        TpoCmb  Z,ZZ9.9999  Tipo de cambio  T/Cambio    0   Tipo de cambio  usuario x(10)   usuario usuario     LugEnt2 x(60)   Lugar de entrega    Lugar de entrega        NroCard x(8)    NroCard Nrocard     Libre_c04   x(60)   Libre_c04       Libre_d01   ->>>,>>>,>>9.99<<<  Libre_d01   0   CodRef  x(3)    Referencia      NroRef  X(12)   No. Referencia  Numero!Referencia       Libre_c01   x(60)   Libre_c01       FaxCli  x(13)   Fax Fax     Fax del Cliente Libre_c02   x(60)   Libre_c02       PorIgv  ->>9.99 % I.G.V.    % I.G.V.    0   Libre_c05   x(60)   Libre_c05       RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  ) :�  ���+������ FAC  00000    ���P                                  �        �        �                �     i     i     i    & 	( 	) 	    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �             '  .  5  :  A  I  Q  Y  c  m  t  {  �  �  �  �  �  �  �  �  �  �    ��                            ����                                 p�                    +V    undefined                                                               �       t�  �   l   ��  ��                    �����               $n�                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                     3   ����       $      H  ���                       8      
                       � ߱        �  �   "   D       �     >          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   |P                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  {  ~  L              �A<                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  �  �  �              �I                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  �  �  p              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  �  �  p              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  �  �  �              4!=                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  �  �  �	              d9S                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  �  �  H              y                     O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  �  �  T              <�P                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  �  �  T              �P                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  �  �  |              �k                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  �  �  |              8�i                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  �  �  |              ��i                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              ��i                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              �D                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              �d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              �e                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              <6f                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              �6f                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              ,i                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              ��7                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              h�d                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              D��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �               -{                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              ��<                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              �:                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              D��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              pf                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                       @$              $xO                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                      l%              D|O                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                      d'              ��{                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                      h(              ��{                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                      �)              `��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                      @+              |�>                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                     #  �,              R                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                  -  2  �-              ��J                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                  4  5  �/              8Y                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                  7  :  �0              �Y                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                  <  =  �1              h�                     O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                  ?  A  �2              XY                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     �       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 �       CHARACTER,  canNavigate �3      �3      (4    �       LOGICAL,    closeQuery  4      44      `4   
 �       LOGICAL,    columnProps @4      l4      �4    �       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7          LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7          LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7          CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    (      CHARACTER,  hasForeignKeyChanged    88      d8      �8    ?      LOGICAL,    openDataQuery   |8      �8      �8    T      LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	 b      LOGICAL,    prepareQuery    9      49      d9    l      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    y      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 �      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 �      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    �      CHARACTER,  assignDBRow                             <  �;      ��                  '  )  <              h�N                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                  +  0  L=              ��N                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                  2  3  �>              @��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                  5  7  �?              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                  9  ;   A              $��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                  =  >  PB              ��D                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                  @  A  PC              4�D                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                  C  D  PD              X��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                  F  G  PE              P��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                  I  J  PF              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  L  N  \G              ���                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  P  Q  �H              (��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  S  U  �I              0��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  W  X  �J              LM�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  Z  [  �K               P�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  ]  `  �L              �P�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O          CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P          CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP    ,      CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P    A      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q     M      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !  Z      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  k      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  z      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  �      CHARACTER,  getForeignValues    @R      lR      �R  %  �      CHARACTER,  getQueryPosition    �R      �R      �R  &  �      CHARACTER,  getQuerySort    �R      �R      S  '  �      CHARACTER,  getQueryString  �R      (S      XS  (  �      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,        CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -        CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  (      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /  9      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0  G      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1  V      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2  f      LOGICAL,    removeQuerySelection    �W      �W      (X  3  w      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  �      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 �      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  �      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              �r�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              �h�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                       �]              xi�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                      �^              �l�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                      `              tm�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  	  
  a              4�4                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                      b              T�4                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                      Dc              ej                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                      Hd               fj                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <        CHARACTER,  getASBound  �d      e      0e  = 
       LOGICAL,    getAsDivision   e      <e      le  >        CHARACTER,  getASHandle Le      xe      �e  ?  (      HANDLE, getASHasStarted �e      �e      �e  @  4      LOGICAL,    getASInfo   �e      �e      f  A 	 D      CHARACTER,  getASInitializeOnRun    �e       f      Xf  B  N      LOGICAL,    getASUsePrompt  8f      df      �f  C  c      LOGICAL,    getServerFileName   tf      �f      �f  D  r      CHARACTER,  getServerOperatingMode  �f      �f      g  E  �      CHARACTER,  runServerProcedure  �f      $g      Xg  F  �      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M        LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N        LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              �c                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              `�z                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              ��n                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              <�Q                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              eQ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              diQ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              jQ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              P'�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t               (�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              �*�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                      �v              �+�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                      �w              (Rf                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                      �x              �Rf                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                      |z              �"D                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                      �{              0sa                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                      �|              �sa                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                       x~              `�:                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                  "  $  �              @�;                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                  &  )  ��              ��;                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                  +  -  L�              �gM                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                  /  0  t�              DlM                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 {      LOGICAL,    assignLinkProperty  ؃      �      8�  P  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  �      CHARACTER,  getChildDataKey ��      ̄      ��  R  �      CHARACTER,  getContainerHandle  ܄      �      <�  S  �      HANDLE, getContainerHidden  �      D�      x�  T  �      LOGICAL,    getContainerSource  X�      ��      ��  U  �      HANDLE, getContainerSourceEvents    ��      ��      ��  V  �      CHARACTER,  getContainerType    ܅      �      <�  W  	      CHARACTER,  getDataLinksEnabled �      H�      |�  X        LOGICAL,    getDataSource   \�      ��      ��  Y  .      HANDLE, getDataSourceEvents ��      ��      �  Z  <      CHARACTER,  getDataSourceNames  Ԇ       �      4�  [  P      CHARACTER,  getDataTarget   �      @�      p�  \  c      CHARACTER,  getDataTargetEvents P�      |�      ��  ]  q      CHARACTER,  getDBAware  ��      ��      �  ^ 
 �      LOGICAL,    getDesignDataObject ȇ      �      (�  _  �      CHARACTER,  getDynamicObject    �      4�      h�  `  �      LOGICAL,    getInstanceProperties   H�      t�      ��  a  �      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  �      CHARACTER,  getObjectHidden �      <�      l�  d  �      LOGICAL,    getObjectInitialized    L�      x�      ��  e        LOGICAL,    getObjectName   ��      ��      �  f        CHARACTER,  getObjectPage   ̉      ��      (�  g  %      INTEGER,    getObjectParent �      4�      d�  h  3      HANDLE, getObjectVersion    D�      l�      ��  i  C      CHARACTER,  getObjectVersionNumber  ��      ��      �  j  T      CHARACTER,  getParentDataKey    Ċ      ��      $�  k  k      CHARACTER,  getPassThroughLinks �      0�      d�  l  |      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  �      CHARACTER,  getPhysicalVersion  ��      ��      �  n  �      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  �      CHARACTER,  getQueryObject  �      4�      d�  p  �      LOGICAL,    getRunAttribute D�      p�      ��  q  �      CHARACTER,  getSupportedLinks   ��      ��      ��  r  �      CHARACTER,  getTranslatableProperties   ��      �      (�  s  �      CHARACTER,  getUIBMode  �      4�      `�  t 
 	      CHARACTER,  getUserProperty @�      l�      ��  u  !	      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  1	      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  F	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  R	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  _	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  k	      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  y	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  �	      CHARACTER,  setChildDataKey 4�      `�      ��  }  �	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  �	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    �	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  
      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  
      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  -
      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  ;
      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 O
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  Z
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  n
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  �
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  �
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  �
      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �        LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  &      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �  9      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �  I      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �  [      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 u      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 �      CHARACTER,INPUT pcName CHARACTER    l�    F  ��  0�      �       4   �����                 @�                      ��                  G  t                  L�W                       G  Ě        H  \�  ؛      �       4   �����                 �                      ��                  I  s                  иW                       I  l�  �    `  �  ��      �       4   �����                 ��                      ��                  l  n                  T�W                       l  �         m                                  ,     
 
                   � ߱        �  $  p  ��  ���                           $  r  @�  ���                       x      
                   � ߱        x�    x  ��  �      �      4   �����                �                      ��                  y  =	                  �W                       y  ��  H�  o   |  
    ,                                 ��  $   }  t�  ���                       �  @         �              � ߱        ��  �   ~        Ȟ  �     �      ܞ  �   �        �  �   �  x      �  �   �  �      �  �   �  `      ,�  �   �  �      @�  �   �        T�  �   �  �      h�  �   �         |�  �   �  |      ��  �   �  �      ��  �   �  t      ��  �   �  �      ̟  �   �  ,      ��  �   �  �      ��  �   �  �      �  �   �  P	      �  �   �  �	      0�  �   �   
      D�  �   �  t
      X�  �   �  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  d	  �	  ��              ��o                    O   ����    e�          O   ����    R�          O   ����    ��      �     
 
                     
               (                         � ߱        h�  $ x	  آ  ���                           O   �	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  �                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  9
                  �f�                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    i
  T�  Ц      x      4   ����x                �                      ��                  j
                    ��                       j
  d�  ��  �   l
  �      �  �   m
  T      �  �   n
  �      0�  �   o
  D      D�  �   p
  �      X�  �   q
  �      l�  �   s
  p      ��  �   t
  �      ��  �   u
  X      ��  �   v
  �      ��  �   w
  �      Ч  �   x
  D       �  �   y
  �       ��  �   z
  �       �  �   {
  x!       �  �   |
  �!      4�  �   }
  h"      H�  �   ~
  �"      \�  �   
  `#      p�  �   �
  �#      ��  �   �
  X$      ��  �   �
  �$      ��  �   �
  �$      ��  �   �
  L%      Ԩ  �   �
  �%      �  �   �
  <&      ��  �   �
  �&      �  �   �
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
  $0      d�      ��  8�      T0      4   ����T0                H�                      ��                    �                   ��                         ̪  \�  �     �0      p�  �     (1      ��  �     �1      ��  �     2      ��  �     �2      ��  �      3      ԫ  �   !  |3      �  �   "  �3      ��  �   #  t4      �  �   $  �4      $�  �   %  l5      8�  �   &  �5      L�  �   '  d6      `�  �   (  �6      t�  �   )  L7      ��  �   *  �7      ��  �   +  <8      ��  �   ,  �8      Ĭ  �   -  ,9      ج  �   .  �9      �  �   /  :       �  �   0  X:      �  �   1  �:      (�  �   2  H;      <�  �   3  �;      P�  �   4  8<          �   5  �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  U                  �o�                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    
  ��  ��      K      4   ����K      /     а     �                          3   ����K             �                      3   ����<K  Զ      ,�  ��  �  XK      4   ����XK  
              ��                      ��             
       w                  ��                         <�  ̱  �     �K      $�  $    ��  ���                       �K     
 
                   � ߱        8�  �     L      ��  $     d�  ���                       ,L  @         L              � ߱        L�  $     ��  ���                       �L      
 	       	           � ߱        �M     
 
               N      
               \O  @        
 O              � ߱        ܳ  V   *  �  ���                        hO      
 	       	       �O      
 
       
       �O      
 	       	           � ߱        l�  $  F  x�  ���                       �P     
 
               Q      
               dR  @        
 $R              � ߱            V   X  �  ���                                      ̵                      ��                  y                    t;                       y  ��  pR     
 
               �R      
               <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   �  �  ���                        adm-clone-props �  ��              �     W     `                          \  �                     start-super-proc    �  d�  �           �     X                                  �                     l�    .  �   �      �X      4   �����X      /   /  ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $  I  ��  ���                       @Y      
                   � ߱        ��    Y  �  \�  ��  \Y      4   ����\Y                и                      ��                  Z  ^                  @p;                       Z  �  pY      
               �Y      
               �Y      
                   � ߱            $  [  l�  ���                             _  �  T�      �Y      4   �����Y  �Y      
                   � ߱            $  `  (�  ���                       �Y      
                   � ߱        ع  $  d  ��  ���                       Ժ    g  ��  �  \�  �Y      4   �����Y      $  h  0�  ���                       Z      
                   � ߱            �   �  ,Z      lZ     
 
               �Z      
               8\  @        
 �[              � ߱         �  V   �  p�  ���                        �  �   �  D\      �    K  0�  @�      �\      4   �����\      /   L  l�     |�                          3   �����\            ��                      3   �����\  �\     
 
               P]      
               �^  @        
 `^              � ߱        ��  V   X  ��  ���                        �^     
 
               h_      
               �`  @        
 x`              � ߱        ̼  V   |  <�  ���                        L�    �  �  d�      �`      4   �����`                t�                      ��                  �                    D,X                       �  ��  �  /      ��     ��                          3   �����`            н                      3   �����`      /     �     �                          3   ����a            <�                      3   ����8a  ��  /  i  x�         la                      3   ����Ta  initProps   x�  ��              ,     Y     $                             *  	                                   ̿          t�  \�      ��                 A  Z  ��              ��G                    O   ����    e�          O   ����    R�          O   ����    ��      4                       ��          ��  p   L   u  �      W  t�  d�     ,u                                        ��                  M  i                  �G                       M  ��   �  ��     @u                                        ��                  j  �                  ��h                       j  ��  ��  |�     Tu                                        ��                  �  �                  ��h                       �  �  �  �     hu                                        ��                  �  �                  |�h                       �  ��  ��  ��     |u                                        ��                  �  �                  L�h                       �  (�  0�   �     �u                                        ��                  �  �                  P�h                       �  ��  ��  ��     �u                                        ��                  �                     �h                       �  @�  H�  8�     �u                                        ��                    4                  ��h                         ��  ��  ��     �u  	                                      ��             	     5  Q                  ��h                       5  X�  `�  P�     �u  
                                      ��             
     R  n                  �h                       R  ��  ��  ��     �u                                        ��                  o  �                  ��h                       o  p�  x�  h�     v                                        ��                  �  �                  d�h                       �  ��  �  ��     v                                        ��                  �  �                  4�h                       �  ��  ��  ��     0v                                        ��                  �  �                  �h                       �  �  �  �     Dv                                        ��                  �  �                  <�k                       �  ��  ��  ��     Xv                                        ��                                       �k                          ,�  4�  $�     lv                                        ��                    9                  ��k                         ��      ��     �v                                        ��                  :  V                  ��k                       :  D�      O   Y  ��  ��  �v                D�          ,�  8�   , �                                                       �      ��                            ����                            ��  d�  ��  �      ��      Z     L�                      � H�  F                     ��    o  �  ��      �v      4   �����v                ��                      ��                  p  �                  ���                       p  �  ��  /   q  ��     ��                          3   �����v            ��                      3   �����v  h�  /   r  (�     8�                          3   �����v            X�                      3   ����w  ��  /   w  ��     ��                          3   ����$w            ��                      3   ����Dw      /   }   �     �                          3   ����dw            0�                      3   �����w  �w     
 
                x      
               py  @        
 0y              � ߱        ��  V   �  @�  ���                        ��  $    ��  ���                       �y      
                   � ߱        �y     
 
               $z      
               t{  @        
 4{              � ߱        ��  V     (�  ���                        t�  $  0  ��  ���                       �{     
 
                   � ߱        �{     
 
               |      
               `}  @        
  }              � ߱        ��  V   :  �  ���                        \�  $  U  ��  ���                       l}     
 
                   � ߱        �}     
 
               �}      
               L  @        
               � ߱        ��  V   _  ��  ���                        D�  $  y  ��  ���                       d      
                   � ߱        �     
 
               �      
               X�  @        
 �              � ߱        p�  V   �  ��  ���                        ��  �   �  p�      @�  $  �  ��  ���                       ��     
 
                   � ߱        ��     
 
                �      
               p�  @        
 0�              � ߱        l�  V   �  ��  ���                        ��  $  �  ��  ���                       |�     
 
                   � ߱        ��  �   �  ��      0�  $  �  �  ���                       Ѓ     
 
                   � ߱        D�  �      �      ��  $  "  p�  ���                       $�      
                   � ߱              -  ��  ��      @�      4   ����@�      /   .  ��     �                          3   ����`�  4�     
   $�                      3   ������  d�        T�                      3   ������  ��        ��                      3   ������            ��                      3   ������  pushRowObjUpdTable  ��  ��  �                   [      �                               o                     pushTableAndValidate    ��  4�  �           |    # \     �                          �  �                     remoteCommit    L�  ��  �           p    $ ]     �                          �  �                     serverCommit    ��  �  �           l    % ^     �                          �  �                                     4�          �  ��      ��                  Q  ^  �              ԫl                    O   ����    e�          O   ����    R�          O   ����    ��          O   \  ��  ��  �    ��                            ����                            $�  P�      ��              _      L�                      
�     �                     disable_UI  ��  ��                      `      �                                  
                    �  �    ����  �       ��    !   ��  8   ����"   ��  8   ����"       8   ����!       8   ����!       ��  ��      viewObject  ,   ��  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  �  4�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  �  x�  ��      returnFocus ,INPUT hTarget HANDLE   h�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  ��  �      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  \�  l�      removeAllLinks  ,   L�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE p�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  t�  ��      hideObject  ,   d�  ��  ��      exitObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  ��  ��      displayLinks    ,   ��  �  �      createControls  ,   ��  (�  8�      changeCursor    ,INPUT pcCursor CHARACTER   �  d�  p�      applyEntry  ,INPUT pcField CHARACTER    T�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  �  �      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  h�  p�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE X�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  �      runServerObject ,INPUT phAppService HANDLE  ��  8�  L�      disconnectObject    ,   (�  `�  p�      destroyObject   ,   P�  ��  ��      bindServer  ,   t�  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  ��  �      startFilter ,   ��  �  (�      releaseDBRow    ,   �  <�  L�      refetchDBRow    ,INPUT phRowObjUpd HANDLE   ,�  x�  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE h�  ��  ��      fetchDBRowForUpdate ,   ��  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  (�  8�      compareDBRow    ,   �  L�  `�      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   <�  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  �  �      updateState ,INPUT pcState CHARACTER    ��  D�  X�      updateQueryPosition ,   4�  l�  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    \�  ��  ��      undoTransaction ,   ��  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  x�  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   h�  ��  �      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  P�  d�      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  @�  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  ,�  @�      startServerObject   ,   �  T�  d�      setPropertyList ,INPUT pcProperties CHARACTER   D�  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ��   �      rowObjectState  ,INPUT pcState CHARACTER    ��  ,�  <�      retrieveFilter  ,   �  P�  d�      restartServerObject ,   @�  x�  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   h�  ��  ��      refreshRow  ,   p�  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  �  $�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  �  T�  l�      initializeServerObject  ,   D�  ��  ��      initializeObject    ,   p�  ��  ��      home    ,   ��  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��   �  �      fetchPrev   ,   ��   �  ,�      fetchNext   ,   �  @�  L�      fetchLast   ,   0�  `�  l�      fetchFirst  ,   P�  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   p�  ��  ��      endClientDataRequest    ,   ��  ��  ��      destroyServerObject ,   ��  �  �      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    ��  h�  x�      dataAvailable   ,INPUT pcRelative CHARACTER X�  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  ��  �      commitTransaction   ,   ��   �  0�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    �  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 _%     adecomm/as-utils.w  
"   
   �    }        �
"     
    �     }        �� Y   <   %               � 
"    
 � %              h �P  \         (          
�                          
�            � �   �
"    
 o
�H T   %              �     }        �GG %              � 
" 
  
   P �L 
�H T   %              �     }        �GG %              
" 
  
   �        �    7%               
" 
  
 ^�               1� �  
 ^� �   � %               o%   o           � �    ^
" 
  
 ^�           �    1� �   ^� �   � %               o%   o           � �   ^
" 
  
 ^�           �    1� �  
 ^� �   � %               o%   o           � �   ^
" 
  
 ^�           l    1� �   ^� �   � %               o%   o           � �    ^
" 
  
 ^�           �    1�    ^� �   � %               o%   o           �    ^
" 
  
 ^�           T    1� )   ^� 5   � %               o%   o           %               
" 
  
 � �          �    1� =   � � M     
" 
  
 ^�               1� T   ^� �   � %               o%   o           � g  ^
" 
  
 ^�           �    1� i   ^� �   � %               o%   o           � x  S ^
" 
  
 ^�           �    1� �   ^� 5   � %               o%   o           %               
" 
  
 ^�           p    1� �   ^� 5   � %               o%   o           %               
" 
  
 ^�           �    1� �   ^� 5   � %               o%   o           %              
" 
  
 � �          h    1� �   � � 5     
" 
  
 ^�           �    1� 
  
 ^� 5   � %               o%   o           %               
" 
  
 ^�                1�    ^� �   � %               o%   o           � �    ^
" 
  
 � �          �    1�    � � M     
" 
  
 ^�           �    1� -   ^� �   � %               o%   o           � C  t ^
" 
  
 � �          D	    1� �  
 � � M     
" 
  
 ^�           �	    1� �   ^� �   � %               o%   o           � �  � ^
" 
  
 ^�           �	    1� a   ^� �   � %               o%   o           � �    ^
" 
  
 ^�           h
    1� x  
 ^� �   � %               o%   o           %               
" 
  
 ��           �
    1� �   �� 5   � %               o%   o           %              
" 
  
 e�           `    1� �   e� �   � %               o%   o           � �    �
" 
  
 e�           �    1� �   e� �   � %               o%   o           o%   o           
" 
  
 o�           P    1� �  
 o� �   � %               o%   o           � �    e
" 
  
 e�           �    1� �   e� �  	 � %               o%   o           � �  / o
" 
  
 � �          8    1�    � � �  	   
" 
  
 e�           t    1�    e� �  	 � o%   o           o%   o           � �    e
" 
  
 � �          �    1� +   � � �  	   
" 
  
 e�           $    1� :   e� �  	 � o%   o           o%   o           � �    e
" 
  
 � �          �    1� J   � � 5     
" 
  
 � �          �    1� X   � � �  	   
" 
  
 � �              1� e   � � �  	   
" 
  
 � �          L    1� r   � � �  	   
" 
  
 o�           �    1� �   o� 5   � o%   o           o%   o           %              
" 
  
 � �              1� �   � � �  	   
" 
  
 � �          @    1� �  
 � � �     
" 
  
 � �          |    1� �   � � �  	   
" 
  
 � �          �    1� �   � � �  	   
" 
  
 � �          �    1� �   � � �  	   
" 
  
 � �          0    1� �   � � �  	   
" 
  
 � �          l    1� �  	 � � �  	   
" 
  
 � �          �    1�    � � �  	   
" 
  
 � �          �    1�    � � �  	   
" 
  
 e�                1� ,   e� �   � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
" 
  
   
" 
  
 
" 
  
   
" 
  
 �(�  L ( l       �        �    �� 8   � P   �        �    �@    
� @  , 
�            �� A     p�               �L
�    %              � 8          � $         � H          
�    � b     
" 
  
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
" 
  
 ��           �    1� e  
 �� �   � %               o%   o           � �    �
" 
  
 ��           <    1� p  
 �� �   � %               o%   o           o%   o           
" 
  
 �           �    1� {   � M   � %               o%   o           o%   o           
" 
  
 e�           4    1� �   e� 5   � %               o%   o           %               
" 
  
 ��           �    1� �   �� 5   � %               o%   o           %               
" 
  
 _�           ,    1� �   _� �   � %               o%   o           � �    �
" 
  
 o�           �    1� �   o� 5   � %               o%   o           %              
" 
  
 o�               1� �   o� 5   � %               o%   o           o%   o           
" 
  
 o�           �    1� �   o� �   � %               o%   o           o%   o           
" 
  
 �               1� �  	 � �   � %               o%   o           � �    e
" 
  
 �           �    1� �   � �   � %               o%   o           o%   o           
" 
  
 ��               1� �   �� �   � %               o%   o           o%   o           
" 
  
 ��           �    1�     �� 5   � %               o%   o           %               
" 
  
 ��           �    1�    �� 5   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
" 
  
 �           �    1�   
 � 5   � %               o%   o           %              
" 
  
 �           H    1� '   � �   � %               o%   o           o%   o           
" 
  
 ��           �    1� 3   �� �   � %               o%   o           � �    o
" 
  
 ��           8    1� A   �� �   � %               o%   o           o%   o           
" 
  
 � �          �    1� M   � � M     
" 
  
 o�           �    1� Z   o� �   � %               o%   o           � m  ! �
" 
  
 e�           d    1� �   e� �   � %               o%   o           � �    o
" 
  
 e�           �    1� �   e� �   � %               o%   o           � �   e
" 
  
 � �          L    1� �   � � �     
" 
  
 � �          �    1� �   � � M     
" 
  
 ��           �    1� �   �� �   � %               o%   o           � �    
" 
  
 � �          8     1� �  
 � � M     
" 
  
 _�           t     1� �   _� 5   � %               o%   o           o%   o           
" 
  
 o�           �     1� 
   o� 5   � %               o%   o           %               
" 
  
 �           l!    1�    � 5   � %               o%   o           %               
" 
  
 o�           �!    1� (   o� �   � %               o%   o           � �    
" 
  
 o�           \"    1� 8   o� �   � %               o%   o           o%   o           
" 
  
 ��           �"    1� D   �� 5   � %               o%   o           %              
" 
  
 e�           T#    1� U   e� 5   � %               o%   o           %               
" 
  
 _�           �#    1� b   _� 5   � %               o%   o           %               
" 
  
 � �          L$    1� r   � � M     
" 
  
 � �          �$    1�    � � �     
" 
  
 o�           �$    1� �   o� �   � %               o%   o           o%   o           
" 
  
 o�           @%    1� �   o� �   � %               o%   o           � �    e
" 
  
 o�           �%    1� �   o� �   � %               o%   o           o%   o           
" 
  
 �           0&    1� �   � 5   � o%   o           o%   o           o%   o           
" 
  
 �           �&    1� �   � �  	 � %               o%   o           o%   o           
" 
  
 ��           ('    1� �   �� �   � %               o%   o           o%   o           
" 
  
 �           �'    1� �  
 � �   � %               o%   o           o%   o           
" 
  
 � �           (    1� �   � � �     
" 
  
 o�           \(    1�    o� �   � %               o%   o           �   4 
" 
  
 e�           �(    1� O  
 e� 5   � %               o%   o           %              
" 
  
 � �          L)    1� Z   � � M     
" 
  
 ��           �)    1� k   �� �   � %               o%   o           � �    _
" 
  
 e�           �)    1� y   e� 5   � %               o%   o           %              
" 
  
 o�           x*    1� �   o� �   � %               o%   o           � �    e
" 
  
 ��           �*    1� �   �� �   � %               o%   o           � �    o
" 
  
 �           `+    1� �   � �   � %               o%   o           � �    �
" 
  
 �           �+    1� �   � 5   � %               o%   o           %               
" 
  
 �           P,    1� �  	 � M   � %               o%   o           o%   o           
" 
  
 _�           �,    1� �   _� �   � %               o%   o           � �  	 e
" 
  
 ��           @-    1� �   �� �   � %               o%   o           %       �       
" 
  
 e�           �-    1� �   e� �   � %               o%   o           � �    �
" 
  
 e�           0.    1� �   e� 5   � o%   o           o%   o           %              
" 
  
 ��           �.    1�    �� 5   � %               o%   o           %               
" 
  
 ��           (/    1�    �� �   � %               o%   o           o%   o           
" 
  
 �           �/    1� .   � �  	 � %               o%   o           � �    �
" 
  
 � �          0    1� ?   � � �  	   P �L 
�H T   %              �     }        �GG %              
" 
  
 _�           �0    1� L  
 _� �   � %               o%   o           � �    _
" 
  
 �           1    1� W   � 5   � %               o%   o           %               
" 
  
 e�           �1    1� d  	 e� �   � %               o%   o           � �    
" 
  
 o�           2    1� n   o� �   � %               o%   o           � �    e
" 
  
 ��           �2    1� |   �� 5   � %               o%   o           %               
" 
  
 �           �2    1� �   � �   � %               o%   o           � �    �
" 
  
 �           p3    1� �   � �   � %               o%   o           o%   o           
" 
  
 ��           �3    1� �   �� �   � %               o%   o           o%   o           
" 
  
 e�           h4    1� �   e� 5   � %               o%   o           o%   o           
" 
  
 _�           �4    1� �   _� 5   � %               o%   o           o%   o           
" 
  
 �           `5    1� �   � 5   � %               o%   o           o%   o           
" 
  
 e�           �5    1� �   e� �   � %               o%   o           o%   o           
" 
  
 ��           X6    1� �  	 �� �  	 � %               o%   o           � �    o
" 
  
 o�           �6    1� �  
 o� �  	 � %               o%   o           � �    �
" 
  
 �           @7    1�    � �   � %               o%   o           � �    o
" 
  
 �           �7    1�    � �   � %               o%   o           o%   o           
" 
  
 ��           08    1� $   �� �   � %               o%   o           o%   o           
" 
  
 ��           �8    1� 1   �� �   � %               o%   o           � �    
" 
  
 e�            9    1� F   e� �   � %               o%   o           � �    �
" 
  
 e�           �9    1� U   e� �  	 � %               o%   o           o%   o           
" 
  
 � �          :    1� g   � � M     
" 
  
 e�           L:    1� s   e� �   � %               o%   o           � �    �
" 
  
 e�           �:    1� �   e� �   � %               o%   o           o%   o           
" 
  
 _�           <;    1� �   _� 5   � %               o%   o           o%   o           
" 
  
 �           �;    1� �  
 � �   � %               o%   o           � �    �
" 
  
 �           ,<    1� �   � �   � %               o%   o           � �    
" 
  
 o�           �<    1� �   o� 5   � %               o%   o           %               P �L 
�H T   %              �     }        �GG %              
" 
  
 o�           p=    1� �  	 o� M   � %               o%   o           o%   o           
" 
  
 o�           �=    1� �   o� M   � %               o%   o           o%   o           
" 
  
 �           h>    1� �   � M   � %               o%   o           o%   o           
" 
  
 _�           �>    1�    _� 5   � %               o%   o           %              
" 
  
 ��           `?    1�    �� �   � %               o%   o           � /  M _
" 
  
 �           �?    1� }   � 5   � %               o%   o           %              
" 
  
 �           P@    1� �   � 5   � %               o%   o           %               
" 
  
 e�           �@    1� �   e� 5   � %               o%   o           %               
" 
  
 ��           HA    1� �   �� �  	 � %               o%   o           � �   e
" 
  
 �           �A    1� �   � 5   � %               o%   o           %               
" 
  
 �           8B    1� �   � �  	 � %               o%   o           o%   o           
" 
  
 ��           �B    1� �   �� 5   � o%   o           o%   o           %              
" 
  
 _�           0C    1�    _� �  	 � o%   o           o%   o           � �    _
" 
  
 ��           �C    1�    �� M   � o%   o           o%   o           o%   o           
" 
  
 ��            D    1� .   �� M   � o%   o           o%   o           o%   o           
" 
  
 ��           �D    1� >   �� �  	 � o%   o           o%   o           o%   o           
" 
  
 ��           E    1� N   �� M   � o%   o           o%   o           o%   o           
" 
  
 ��           �E    1� ]   �� �  	 � o%   o           o%   o           � k   �
" 
  
 �           F    1� m   � �  	 � o%   o           o%   o           � |   
" 
  
 e�           |F    1� �   e� 5   � %               o%   o           %               
" 
  
 e�           �F    1� �   e� 5   � %               o%   o           %               
" 
  
 � �          tG    1� �   � � �  	   
" 
  
 o�           �G    1� �   o� 5   � %               o%   o           %               
" 
  
 o�           ,H    1� �   o� �   � %               o%   o           o%   o           
" 
  
 ��           �H    1� �   �� �   � %               o%   o           o%   o           
" 
  
 �           $I    1� �   � 5   � %               o%   o           o%   o           
" 
  
 �           �I    1� 
   � �   � %               o%   o           � �    
" 
  
 ��           J    1�    �� '   � %               o%   o           %               
" 
  
 e�           �J    1� /  	 e� 5   � %               o%   o           %                " 
   � %     start-super-proc � %     adm2/smart.p .�P �L 
�H T   %              �     }        �GG %              
" 
  
   �       �K    6� 8     
" 
  
   
�        �K    8
" 
  
   �        �K    ��     }        �G 4              
" 
  
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
" 
  
 �
" 
  
 � 
" 
  
 �
" 
  
   (�  L ( l       �        �M    �� 8   � P   �        �M    �@    
� @  , 
�       �M    �� A   �p�               �L
�    %              � 8       N    � $         � H          
�    � b   �
" 
  
 �p� @  , 
�       O    �� T   �p�               �L" 
 	  , �   � e   � g   � �     }        �A      |    " 
 	    � e   �%              (<   \ (    |    �     }        �A� i   �A" 
 
      " 
 	  �" 
 
    < " 
 	  �" 
 
  (    |    �     }        �A� i   �A" 
 
  
�H T   %              �     }        �GG %              
" 
  
 �
" 
  
 � 
" 
  
 �
" 
  
   (�  L ( l       �        �P    �� 8   � P   �        �P    �@    
� @  , 
�       �P    �� A   �p�               �L
�    %              � 8      Q    � $         � H          
�    � b   �
" 
  
 �p� @  , 
�       R    �� �  
 �p�               �L" 
 	  , 
�H T   %              �     }        �GG %              
" 
  
   
" 
  
 �
" 
  
   
" 
  
   (�  L ( l       �        �R    �� 8   � P   �        �R    �@    
� @  , 
�       �R    �� A     p�               �L
�    %              � 8      �R    � $         � H          
�    � b     
" 
  
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
" 
  
   p� @  , 
�       XT    �� �     p�               �L%               
" 
  
  p� @  , 
�       �T    �� :    p�               �L%               
" 
  
  p� @  , 
�       U    ��     p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 �    �        �U    �� 8   �
"   
   � 8      DV    � $         � H          
�    � b   �
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6� 8     
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � �   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 �"      �       }        �
"   
 � %              %                " 
   � %     start-super-proc � %     adm2/appserver.p ���    �      
�    �     }        �%               %      Server  - �     }        �    " 
   e� �    � %               %      Client      " 
   e� �    � %      NONE    p�,  8         $     " 
           � 4   �
�    
�H T   %              �     }        �GG %              
" 
  
 �
" 
  
 � 
" 
  
 �
" 
  
   (�  L ( l       �        �Z    �� 8   � P   �        �Z    �@    
� @  , 
�       �Z    �� A   �p�               �L
�    %              � 8      �Z    � $         � H          
�    � b   �
" 
  
 �p� @  , 
�       �[    �� �   �p�               �L" 
   , p�,  8         $     " 
           � B   �
�     " 
   � %     start-super-proc � %     adm2/dataquery.p �e
�H T   %              �     }        �GG %              
" 
  
 �
" 
  
 � 
" 
  
 �
" 
  
 �(�  L ( l       �         ]    �� 8   � P   �        ,]    �@    
� @  , 
�       8]    �� A   �p�               �L
�    %              � 8      D]    � $         � H   �     
�    � b   �
" 
  
 �p� @  , 
�       T^    �� �   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
" 
  
 �
" 
  
 � 
" 
  
 �
" 
  
 �(�  L ( l       �        8_    �� 8   � P   �        D_    �@    
� @  , 
�       P_    �� A   �p�               �L
�    %              � 8      \_    � $         � H   �     
�    � b   �
" 
  
 �p� @  , 
�       l`    �� )   �p�               �L%               " 
   � %     start-super-proc � %     adm2/query.p .�%     start-super-proc � %     adm2/queryext.p % 	    initProps �
�    %� � �   FOR EACH FacCPedi       WHERE FacCPedi.CodCia = 1  AND FacCPedi.CodDoc = "COT"  AND FacCPedi.CodDiv = "00001" NO-LOCK INDEXED-REPOSITION    �   �      �      �      
�H T   %              �     }        �GG %              
" 
  
 �
" 
  
 � 
" 
  
 �
" 
  
   (�  L ( l       �        �b    �� 8   � P   �        �b    �@    
� @  , 
�       �b    �� A   �p�               �L
�    %              � 8      �b    � $         � H          
�    � b   �
" 
  
 �p� @  , 
�       �c    �� �   �p�               �L"    ,     "    e�     � 
�H T   %              �     }        �GG %              
" 
  
 �
" 
  
 � 
" 
  
 �
" 
  
   (�  L ( l       �        |d    �� 8   � P   �        �d    �@    
� @  , 
�       �d    �� A   �p�               �L
�    %              � 8      �d    � $         � H          
�    � b   �
" 
  
 �p� @  , 
�       �e    �� d  	 �p�               �L"    , %               �    "      �          %              %                   "      %                  "      "      "     T(        "    i%              "    i�    � "      �       "    ��    "    i� i   � � �      � i   ��    "     � i    S    "      "    �     "    w%                � @    �     t T     P   4       � "      (0       4       /"      � �      � �    ��    /T ,  %              T   "    /"    � �      � i   ��    /T    �    "    /� i   � "      � i   �"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    �%              � �    � �      4  /     "      
�H T   %              �     }        �GG %              
" 
  
 �
" 
  
 � 
" 
  
 �
" 
  
   (�  L ( l       �        xj    �� 8   � P   �        �j    �@    
� @  , 
�       �j    �� A   �p�               �L
�    %              � 8      �j    � $         � H          
�    � b   �
" 
  
 �p� @  , 
�       �k    �� L  
 �p�               �L"    ,       "  
  ��    �   e�    �       "  	    �    �   � �    e�   �      �      �   ��   �      �    ��   e�   �      �      �     
�H T   %              �     }        �GG %              
" 
  
 � 
" 
  
 �
" 
  
 � 
" 
  
 � (�  L ( l       �        <m    �� 8   � P   �        Hm    �@    
� @  , 
�       Tm    �� A   � p�               �L
�    %              � 8      `m    � $         � H          
�    � b     
" 
  
 �p� @  , 
�       pn    �� �   �p�               �L"    , 
" 
  
   p� @  , 
�       �n    �� �     p�               �L"    , 
" 
  
  p� @  , 
�        o    �� �    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    �     �          "  	  /�     "    wT    "      "      @ A,    �   �    � �      "    �"       T      @   "    � (        "      � �    �� �      �    �"    �     "  	   %              D H   @ A,    �   �    ��      "    �"    �,    S   "    ��   ��    � %                T      @   "    � (        "      � �    �� �      �    �"    e     "  
   %                         "    � �      "    �           "      �    �"      
�H T   %              �     }        �GG %              
" 
  
 w
" 
  
   
" 
  
 w
" 
  
 �(�  L ( l       �        <s    �� 8   � P   �        Hs    �@    
� @  , 
�       Ts    �� A   wp�               �L
�    %              � 8      `s    � $         � H   �     
�    � b   � 
" 
  
 �p� @  , 
�       pt    �� �   �p�               �L"    , 
" 
  
   p� @  , 
�       �t    �� �     p�               �L"    , "       %              %              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "        " 
   � %     start-super-proc � %     adm2/data.p %     start-super-proc � %     adm2/dataext.p %     start-super-proc � %     adm2/dataextcols.p %     start-super-proc � %     adm2/dataextapi.p �
�H T   %              �     }        �GG %              
" 
  
 �
" 
  
 � 
" 
  
 �
" 
  
 �(�  L ( l       �        �w    �� 8   � P   �        �w    �@    
� @  , 
�       x    �� A   �p�               �L
�    %              � 8      x    � $         � H   �     
�    � b   �
" 
  
 �p� @  , 
�       $y    �� �   �p�               �L%               %      "pruebas/dcotcredito.i" 
�H T   %              �     }        �GG %              
" 
  
 �
" 
  
 � 
" 
  
 �
" 
  
   (�  L ( l       �        �y    �� 8   � P   �         z    �@    
� @  , 
�       z    �� A   �p�               �L
�    %              � 8      z    � $         � H          
�    � b   �
" 
  
 �p� @  , 
�       ({    �� �   �p�               �L" 
   , 
�     	         �G
�H T   %              �     }        �GG %              
" 
  
 �
" 
  
 � 
" 
  
 �
" 
  
   (�  L ( l       �        �{    �� 8   � P   �        �{    �@    
� @  , 
�       �{    �� A   �p�               �L
�    %              � 8      |    � $         � H          
�    � b   �
" 
  
 �p� @  , 
�       }    �� �  
 �p�               �L
" 
  
 , 
�     
   !     �G
�H T   %              �     }        �GG %              
" 
  
 �
" 
  
 � 
" 
  
 �
" 
  
   (�  L ( l       �        �}    �� 8   � P   �        �}    �@    
� @  , 
�       �}    �� A   �p�               �L
�    %              � 8      �}    � $         � H          
�    � b   �
" 
  
 �p� @  , 
�            �� �  	 �p�               �L
" 
  
 , 
" 
  
 �      �   	   �        X    �
�H T   %              �     }        �GG %              
" 
  
 �
" 
  
 � 
" 
  
 �
" 
  
   (�  L ( l       �        �    �� 8   � P   �        �    �@    
� @  , 
�       �    �� A   �p�               �L
�    %              � 8      �    � $         � H          
�    � b   �
" 
  
 �p� @  , 
�       �    �� (   �p�               �L" 
   , 
" 
  
   �       d�    �" 
     
�     
   "     �G
�H T   %              �     }        �GG %              
" 
  
 �
" 
  
 � 
" 
  
 �
" 
  
   (�  L ( l       �        ��    �� 8   � P   �        ��    �@    
� @  , 
�       �    �� A   �p�               �L
�    %              � 8      �    � $         � H          
�    � b   �
" 
  
 �p� @  , 
�       $�    �� �  	 �p�               �L
" 
  
 , 
�        !     �Gp�,  8         $     
" 
  
 w        � "   �
�    
�        "     �Gp�,  8         $     
" 
  
 w        � 4   �
�    �    � F     
�        " 
   e� �    � %     modifyListProperty  
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    " #     �  %      setContextAndInitialize 
�    " $     %     bufferCommit    
�    " $     " $     �    � �     
�    %               %     bufferCommit    
�    " %     " %     
�     
   "     �G�     }        �
�                    �           �   l       ��                 �  �  �               �w                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       �U     
                    � ߱              �  (  �      V      4   ����V                �                      ��                  �  �                  L�i                       �  8  �  �  �  PV            �  �  `      �V      4   �����V                p                      ��                  �  �                  ؚi                       �  �  �  o   �      ,                                 �  �   �  �V      �  �   �  �V      $  $  �  �  ���                        W     
                    � ߱        8  �   �  @W      L  �   �  `W      `  �   �  �W          $   �  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �    �               �!X                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  ('X                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �      �X          O     ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��                 u  4  �               Pi;                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       ta                         � ߱        �  $  �  8  ���                       b                         � ߱        8b     
 
               �b      
               d  @        
 �c              � ߱        �  V   �  d  ���                        �        �      d      4   ����d  0d     
 
               �d      
               �e  @        
 �e              � ߱            V        ���                          $  @  �  ���                       f                         � ߱        �  $  A  4  ���                       f                         � ߱          �      4  8                      ��        0         C  Y                  |]      �f     �     C  `      $  C    ���                       <f                         � ߱        �  $  C  `  ���                       lf                         � ߱            4   �����f  �f                     g                     g                     dg                     �g                         � ߱        d  $  D  �  ���                             Q  �  �      �g      4   �����g      $  R  �  ���                       �g          �h             � ߱        �  $  \    ���                       i                         � ߱          �        |                      ��        0         ^  c                  �]      �i     8     ^  @      $  ^  �  ���                       i                         � ߱        l  $  ^  @  ���                       Hi                         � ߱            4   ����pi      $  `  �  ���                       �i                         � ߱        ,j     
 
               �j      
               �k  @        
 �k              � ߱        �  V   n  �  ���                        l       
       
       8l       	       	       ll                     �l                         � ߱        	  $  �  d  ���                       
  $  Y  <	  ���                       �l                         � ߱        �l     
 
               lm      
               �n  @        
 |n          o  @        
 �n          lo  @        
 ,o              � ߱        �
  V   e  h	  ���                          �
        |                      ��        0         �  �                  ,�G      �o     T     �  0
      $  �  �
  ���                       xo                         � ߱        \  $  �  0  ���                       �o                         � ߱        l  4   �����o      4   ����p  �  $  �  �  ���                       pp                         � ߱        �    �  �  l      �p      4   �����p                �                      ��                  �  �                  ��G                       �     �p                     <q       	       	           � ߱            $  �  |  ���                             �    �      dq      4   ����dq                �                      ��                  �  �                  L�G                       �    �q                     `r       
       
           � ߱            $  �  �  ���                       �r                     �r                         � ߱           $  �    ���                       �r     
 
               ls      
               �t  @        
 |t          u  @        
 �t              � ߱            V   �  �  ���                                    7 �          �  d  � `                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             �   �   �   �   �   �   �   �       $  4  D  T  d  t  �  \  �  �  �  �  8HXhx��������(8HX      �   �   �   �   �   �   �   �      $  4  D  T  d  t  �   \ �  �  �  �  8HXhx��������(8HX  �                    � �                     �          ��                            ����                                                        �   l       ��                  �  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��              !       "             ��                            ����                                            �           �   l       ��                  �  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      �   #    �              �                  $       "           d  /  �  $     4  ��                      3   ����Ԅ            T                      3   ������      O   �  ��  ��  �             #  �          �  �    �                                        #     ��                            ����                                            H          �   l       ��                  �    �               ��l                    O   ����    e�          O   ����    R�          O   ����    ��      �   $    �              �                $       "           �   $    ,             �          �   $                             �  /    t     �  ,�                      3   �����            �                      3   ����4�     /    �     �  \�                      3   ����@�  x                             3   ����d�      $     L  ���                                $                   � ߱                  �  �                  3   ����p�      $     �  ���                                $                   � ߱        X  $    ,  ���                       |�      $                   � ߱            O     ��  ��  ��             $  �          �  �   @ �                                                              0              0      $     ��                            ����                                                       �   l       ��                  #  D  �               p�l                    O   ����    e�          O   ����    R�          O   ����    ��      �       $       "           �   %                 �          �   %                   �              /  A  L     \  ȅ                      3   ������  �        |  �                  3   ����Ѕ      $   A  �  ���                                %                   � ߱                                      3   ����܅      $   A  @  ���                                %                   � ߱                   %  �          �  �   , �                                                            %     ��                            ����                                            �           �   l       ��                  �    �               A                    O   ����    e�          O   ����    R�          O   ����    ��              �   �       ��      4   ������      �     �    ��                            ����                            TXS appSrvUtils S-CODCIA S-USER-ID S-CODDOC S-CODDIV S-NROSER s-TpoPed pCodDiv cl-codcia .\aplic\pruebas\dcotcredito.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "pruebas/dcotcredito.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server Client NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH FacCPedi       WHERE FacCPedi.CodCia = 1  AND FacCPedi.CodDoc = "COT"  AND FacCPedi.CodDiv = "00001" NO-LOCK INDEXED-REPOSITION ,   FacCPedi  ; Atencion Cmpbnte CodCia CodCli CodDiv CodDoc CodMon CodPos CodVen DirCli FchEnt FchPed fchven FlgEst FlgIgv FmaPgo Glosa LugEnt NomCli NroPed ordcmp RucCli Sede TpoCmb usuario LugEnt2 NroCard Libre_c04 Libre_d01 CodRef NroRef Libre_c01 FaxCli Libre_c02 PorIgv Libre_c05 INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p Atencion Cmpbnte CodCia CodCli CodDiv CodDoc CodMon CodPos CodVen DirCli FchEnt FchPed fchven FlgEst FlgIgv FmaPgo Glosa LugEnt NomCli NroPed ordcmp RucCli Sede TpoCmb usuario LugEnt2 NroCard Libre_c04 Libre_d01 CodRef NroRef Libre_c01 FaxCli Libre_c02 PorIgv Libre_c05 RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery x  �3  �  hA      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   x	  �	  �	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �                  �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable             |     cQueryString    |  �  "   Y   �          �                  initProps   �  �  �      @  A  C  D  Q  R  Y  \  ^  `  c  n  �  Y  e  �  �  �  �  �  �  �  �  �  �  �  �  �  4             d     lRet               �        piTableIndex    �  �  (   Z   P  l      �                  deleteRecordStatic  L  M  i  j  �  �  �  �  �  �  �  �  �  �      4  5  Q  R  n  o  �  �  �  �  �  �  �  �  �         9  :  V  W  Y  Z      "           !       �  �     [       x      �                  pushRowObjUpdTable  �  �  #      �        pcValType       "           $       �  \     \       �      D                  pushTableAndValidate    �  �  �  �  $      �        pcContext   �  "           $       �  $      �        pcMessages      $      �        pcUndoIds     ,     ]       h                        remoteCommit              X  "           $       |  %      p        pcMessages      %      �        pcUndoIds   �  �     ^       @      �                  serverCommit    A  D  �  ,     _                                 getRowObjUpdStatic  \  ^  �  p     `               d                  disable_UI      4  �$       �      �$                        �  �  )   RowObject   �         �         �         �         �         �         �         �         �         �                                             $         ,         4         <         D         L         T         \         d         l         t         |         �         �         �         �         �         �         �         �         �         �         �         �         �                           Atencion    Cmpbnte CodCia  CodCli  CodDiv  CodDoc  CodMon  CodPos  CodVen  DirCli  FchEnt  FchPed  fchven  FlgEst  FlgIgv  FmaPgo  Glosa   LugEnt  NomCli  NroPed  ordcmp  RucCli  Sede    TpoCmb  usuario LugEnt2 NroCard Libre_c04   Libre_d01   CodRef  NroRef  Libre_c01   FaxCli  Libre_c02   PorIgv  Libre_c05   RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     ,  8  *   RowObjUpd   0         <         D         L         T         \         d         l         t         |         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                    $         ,         4         @         H         T         \         h         p         |         �         �         �         Atencion    Cmpbnte CodCia  CodCli  CodDiv  CodDoc  CodMon  CodPos  CodVen  DirCli  FchEnt  FchPed  fchven  FlgEst  FlgIgv  FmaPgo  Glosa   LugEnt  NomCli  NroPed  ordcmp  RucCli  Sede    TpoCmb  usuario LugEnt2 NroCard Libre_c04   Libre_d01   CodRef  NroRef  Libre_c01   FaxCli  Libre_c02   PorIgv  Libre_c05   RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   �          �  
   appSrvUtils �        �     S-CODCIA                  S-USER-ID   ,              S-CODDOC    L        @     S-CODDIV    l        `     S-NROSER    �        �     s-TpoPed    �        �     pCodDiv �  	 	     �     cl-codcia   �  
 
    �     xiRocketIndexLimit              
   gshAstraAppserver   @         ,   
   gshSessionManager   d         T   
   gshRIManager    �         x   
   gshSecurityManager  �         �   
   gshProfileManager   �         �   
   gshRepositoryManager    !        �   
   gshTranslationManager   0!         !  
   gshWebManager   T!        D!     gscSessionId    x!        h!     gsdSessionObj   �!        �!  
   gshFinManager   �!        �!  
   gshGenManager   �!        �!  
   gshAgnManager   "        �!     gsdTempUniqueID ("        "     gsdUserObj  P"        <"     gsdRenderTypeObj    x"        d"     gsdSessionScopeObj  �"  
 
    �"  
   ghProp  �"  
 
    �"  
   ghADMProps  �"  
 
    �"  
   ghADMPropsBuf    #  
 
    �"     glADMLoadFromRepos  #  
 
    #     glADMOk <#  
 
    0#  
   ghContainer \#  
 
 	   P#     cObjectName x#  
 
 
   p#     iStart  �#  
 
    �#     cAppService �#  
 
    �#     cASDivision �#  
 
    �#     cServerOperatingMode    $  
 
    �#     cContainerType  ,$  
 
    $     cQueryString    L$  
 
    @$  
   hRowObject  l$  
 
    `$  
   hDataQuery  �$  
 
    �$     cColumns        
 
    �$     cDataFieldDefs  �$   ! X  �$  RowObject        " X  �$  RowObjUpd          "   >   �   �   �   �   F  G  H  I  `  l  m  n  p  r  s  t  x  y  |  }  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  =	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  9
  i
  j
  l
  m
  n
  o
  p
  q
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
  �
  �
  �
  �
  �
  �
  �
  �
                     !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /  0  1  2  3  4  5  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  U  
                   *  F  X  w  y  �    .  /  I  Y  Z  [  ^  _  `  d  g  h  �  �  �  K  L  X  |  �  �         i  o  p  q  r  w  }  �  �      0  :  U  _  y  �  �  �  �  �  �  �     "  -  .      �W  .\aplic\pruebas\dcotcredito.w    )  ��  C:\Progress\OpenEdge\src\adm2\data.i @)  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    p)  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i �)  +� , .\aplic\pruebas\dcotcredito.i    �)  �:   C:\Progress\OpenEdge\src\adm2\query.i    *  z + C:\Progress\OpenEdge\src\adm2\delrecst.i D*  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  x*   ) %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   �*  � ! C:\Progress\OpenEdge\src\adm2\dataquery.i    �*  �Z ( %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   $+  �< " C:\Progress\OpenEdge\src\adm2\appserver.i    h+  �� ' %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �+  I� # C:\Progress\OpenEdge\src\adm2\smart.i    �+  Ds & C:\Progress\OpenEdge\gui\fn  ,  tw % %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   @,  Q. $ C:\Progress\OpenEdge\gui\set �,  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i �,  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    �,  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i     -  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  d-  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i �-  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i �-   
 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    .  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   T.  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   �.  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �.  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    /  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    \/  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �/  �j  C:\Progress\OpenEdge\gui\get �/  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �/  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    @0  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �0  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �0  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �0  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   ,1  �  C:\Progress\OpenEdge\src\adm2\appsprto.i p1  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �1  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �1  ª  %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   (2  �� 	 C:\Progress\OpenEdge\src\adm2\qryprto.i  p2  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   �2  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �2  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    3  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   `3  �    !C:\newsie\on_in_co\aplic\pruebas\dcotcredito_cl.w           =      �3  �   f     �3  [       �3       &   4  �   �     4     6  .   (4  �   ,     84          H4  �   
     X4     �  $   h4  �   �     x4     �  $   �4  �   �     �4     �  $   �4  �   �     �4     {  $   �4  �   y     �4     W  $   �4  �   T     �4     2  $   5  �   0     5       $   (5  �        85     �  $   H5  �   �     X5     �  -   h5  �   �     x5     �  ,   �5  k   b     �5  �  V      �5     <  +   �5  �  9      �5       +   �5  �        �5       +   �5  �  �      6     �  +   6  �  �      (6     �  +   86  �  �      H6     �  +   X6  �  �      h6     �  +   x6  �  �      �6     q  +   �6  �  n      �6     T  +   �6  �  Q      �6     7  +   �6  �  4      �6       +   �6  �        7     �  +   7  �  �      (7     �  +   87  �  �      H7     �  +   X7  �  �      h7     �  +   x7  �  �      �7     �  +   �7  �  �      �7     l  +   �7  �  i      �7     O  +   �7  �  /      �7       $   �7  �        8     �  $   8  k  �      (8     �  $   88  j  �      H8     �  $   X8  i        h8     ]  $   x8  _  S      �8     -  *   �8  ^  ,      �8       *   �8  ]        �8     �  *   �8  \  �      �8     �  *   �8  [  �      9     �  *   9  Z  �      (9     j  *   89  Y  i      H9     C  *   X9  X  B      h9       *   x9  W        �9     �  *   �9  V  �      �9     �  *   �9  U  �      �9     �  *   �9  T  �      �9     �  *   �9  S        :     Y  *   :  R  X      (:     2  *   8:  Q  1      H:       *   X:  P  
      h:     �  *   x:  O  �      �:     �  *   �:  N  �      �:     �  *   �:  @  �      �:     f  $   �:    5      �:       $   �:          ;     �  $   ;  �   e      (;       )   8;  g   �      H;  a   �  !   X;     �  (   h;  _   �  !   x;     t  $   �;  ]   r  !   �;     P  $   �;  I   <  !   �;  �   3  "   �;     �  '   �;  �   �  "   �;     �  $   �;  �   �  "   <     �  $   <  �   �  "   (<     m  $   8<  g   S  "   H<     4     X<  O     "   h<  �   �  #   x<     �  &   �<  �   t  #   �<       %   �<  �     #   �<     �  $   �<  �   �  #   �<     �  $   �<  �   �  #   �<     �  $   =  �   �  #   =     �  $   (=  �   r  #   8=     P  $   H=  }   D  #   X=     "  $   h=     �  #   x=     X  "   �=       !   �=     �      �=     ^     �=  �   U     �=  O   G     �=     6     �=     �     �=  �   �     >  �   �     >  O   �     (>     �     8>     9     H>  y        X>  �     
   h>  G   �
     x>     �
     �>     �
     �>  c   A
  
   �>  x   9
     �>  M   $
     �>     
     �>     �	     �>  a   �	     �>  �  �	     ?     p	     ?  �  =	     (?  O   /	     8?     	     H?     �     X?  �   �     h?     �     x?     !     �?  x        �?          �?     �     �?     �     �?     s     �?     Z     �?  Q   J     �?     �     @     �     @     �     (@     �     8@  ]   �  
   H@     z     X@     2  
   h@     $     x@       
   �@  Z   �     �@       	   �@     �     �@     �     �@     �     �@  c   �     �@     l     �@     $     A          A     �      (A     �      8A     &      HA           XA           