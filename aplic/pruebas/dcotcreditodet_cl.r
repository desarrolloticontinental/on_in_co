	��VulZY`9   �                                              a1 396000EFutf-8 MAIN C:\newsie\on_in_co\aplic\pruebas\dcotcreditodet_cl.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,AftIgv logical 0 0,AftIsc logical 1 0,CanPed decimal 2 0,CodCia integer 3 0,CodCli character 4 0,CodDiv character 5 0,CodDoc character 6 0,codmat character 7 0,Factor decimal 8 0,FchPed date 9 0,FlgEst character 10 0,ImpDto decimal 11 0,ImpDto2 decimal 12 0,ImpIgv decimal 13 0,ImpIsc decimal 14 0,ImpLin decimal 15 0,MrgUti decimal 16 0,NroItm integer 17 0,NroPed character 18 0,Pesmat decimal 19 0,PorDto decimal 20 0,PorDto2 decimal 21 0,PreBas decimal 22 0,PreUni decimal 23 0,UndVta character 24 0,DesMat character 25 0,DesMar character 26 0,TipVta character 27 0,canate decimal 28 0,Por_Dsctos1 decimal 29 0,Por_Dsctos2 decimal 30 0,Por_Dsctos3 decimal 31 0,Libre_c05 character 32 0,RowNum integer 33 0,RowIdent character 34 0,RowMod character 35 0,RowIdentIdx character 36 0,RowUserProp character 37 0,ChangedFields character 38 0      5              !             i� 5  �              ��              �?     +   �� �  W   4� `  X   ��   Y   ��   [   ��   \   �� <  ]   �    ^   (� 0  `   ? X� !  iSO8859-1                                                                           h4    �                                      �                   ��                �4  �       >   T�              ��  �   �4      �4                                                         PROGRESS                         �           
    
                    �              �                                                                                                     
  H  �      �  
    
                  �  x             4                                                                                          �          
  �  �      p  
    
                  \  $             �                                                                                          �          
  �          
    
                    �             �                                                                                                    
  L        �  
    
                  �  |             8                                                                                                    
  �  +      t  
    
                  `  (             �                                                                                          +          
  �  =         
    
                    �             �                                                                                          =          
  P  R      �  
    
                  �  �  	           <                                                                                          R          
  �  h      x  
    
                  d  ,  
           �                                                                                          h          
  �  v      $                           �             �                                                                                          v            T  �      �                        �  �             @                                                                                          �             	  �      |  
    
                  h  0	             �                                                                                          �          
  �	  �      (	  
    
                  	  �	             �	                                                                                          �          
  X
  �      �	  
    
                  �	  �
             D
                                                                                          �          
    �      �
                        l
  4             �
                                                                                          �            �  �      ,                          �             �                                                                                          �            \  �      �                        �  �             H                                                                                          �                �      �                        p               �                                                                                          �            p         �       x  X  �   E   !  x  r�      `!  &       x             �                �              �       �  X  (3  F   p3  �  �*      �3  '       �         �    �!          %      �                 P�                                               T�          `  �  L l@                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                                 �  �  �  �  �                         �  �  �  �  �                                0  $          4             H  P  T  \  X          `             t  |  �  �  �          �             �  �  �  �  �          �             �                                    (  0  P  @                         T  \  h  x  p          |             �  �  �  �  �                         �  �  �  �                             �  �  �    �                             ,  4                             8  @  P  h  \                         l  t  �  �  �                         �  �  �  �  �                         �  �  �    �                               (                            ,  4  <  X  H                         \  d  t  �  |                         �  �  �  �  �                         �  �  �  �  �                         �  �  �                                 ,  L  <                         P  X  `  l  h          p             �  �  �  �  �          �             �  �  �     �                               ,                            0  8  H  `  T          d             x  �  �  �  �                         �  �  �  �  �                         �  �  �  �  �                         �  �                                         (   0                              4   @   H   T                               X   `   h   p                              t   �   �   �                              �   �   �   �                                                                           AftIgv  Si/No   I.G.V.  I.G.V.  Si  AftIsc  Si/No   I.S.C.  I.S.C.  No  CanPed  >,>>>,>>9.9999  Cantidad    Cantidad    0   Cantidad despachada CodCia  999 Cia Cia 0   C�digo de compa�ia  CodCli  x(8)    Codigo  Codigo      C�digo del Cliente  CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CodDoc  x(3)    Codigo  Codigo      codmat  X(6)    Codigo Articulo Codigo Articulo     Factor  >>,>>9.9999 Factor  Factor  0   Factor  FchPed  99/99/9999  Fecha   Fch.Pedido  today   FlgEst  X(1)    FlgEst  P   ImpDto  >,>>>,>>9.9999  Importe Descuento   Importe!Descuento   0   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   ImpIgv  >,>>>,>>9.9999  Importe Igv Importe Igv 0   ImpIsc  >,>>>,>>9.9999  Importe Isc Importe Isc 0   ImpLin  ->>,>>>,>>9.99  Importe Importe 0   MrgUti  ->>,>>9.99  Margen de Utilidad  Margen de! Utilidad 0   NroItm  >>9 No.Item No.Item 0   NroPed  X(12)   No. Pedido  Numero!Pedido       Pesmat  ->>,>>9.9999    Peso    Peso    0   PorDto  >>9.99  % Dscto.    % Dscto.    0   PorDto2 >>9.99  % Dscto.    % Dscto.    0   PreBas  >,>>>,>>9.9999  Precio Base Precio Base 0   PreUni  >,>>>,>>9.99999 Precio Unitario Precio Unitario 0   UndVta  x(8)    Unidad  Und     Unidad de movimiento    DesMat  X(45)   Descripci�n Descripci�n     Descripci�n del material    DesMar  X(30)   Des.Marca   Descripcion!Marca       TipVta  X(1)    Tipo Venta  Tipo venta      canate  >,>>>,>>9.9999  Cantidad    Cantidad    0   Cantidad atendida   Por_Dsctos1 ->>9.99 % Dscto % Dscto 0   Por_Dsctos2 ->>9.99 % Dscto % Dscto 0   Por_Dsctos3 ->>9.99 % Dscto % Dscto 0   Libre_c05   x(60)   Libre_c05       RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  & 6�  ���'������    00000   �P                                  �        �        �                �     i     i     i    # 	% 	& 	    �  �  �  �  �  �  �  �  �  �  �  �  �  �          "  )  0  7  ?  F  M  T  [  b  i  p  |  �  �  �  �  �  �  �                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                                 �+   ,  ,  ,  ,                         ,  $,  ,,  <,  4,                         @,  H,  X,  p,  d,          t,             �,  �,  �,  �,  �,          �,             �,  �,  �,  �,  �,          �,             �,  �,  �,  -  -          -             <-  D-  L-  \-  T-                         `-  h-  p-  �-  �-                         �-  �-  �-  �-  �-          �-             �-  �-  �-  �-  �-                         �-  �-  .  .                             .  .  (.  P.  <.                         T.  \.  l.  t.                             x.  �.  �.  �.  �.                         �.  �.  �.  �.  �.                         �.  �.  �.  /   /                         /  /   /  H/  4/                         L/  T/  X/  h/  `/                         l/  t/  |/  �/  �/                         �/  �/  �/  �/  �/                         �/  �/  �/  �/  �/                         �/  �/  0  0  0                          0  (0  80  P0  D0                         T0  \0  l0  �0  |0                         �0  �0  �0  �0  �0          �0             �0  �0  �0  �0  �0          �0             1  1   1  @1  ,1                         D1  L1  T1  l1  `1                         p1  x1  �1  �1  �1          �1             �1  �1  �1  �1  �1                         �1  �1  �1  2  �1                         2  2  2  ,2  $2                         02  <2  D2  P2                             T2  \2  h2  p2                             t2  �2  �2  �2                              �2  �2  �2  �2                             �2  �2  �2  �2                             �2  �2  �2  �2                              �2  3  3  $3                                                                          AftIgv  Si/No   I.G.V.  I.G.V.  Si  AftIsc  Si/No   I.S.C.  I.S.C.  No  CanPed  >,>>>,>>9.9999  Cantidad    Cantidad    0   Cantidad despachada CodCia  999 Cia Cia 0   C�digo de compa�ia  CodCli  x(8)    Codigo  Codigo      C�digo del Cliente  CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CodDoc  x(3)    Codigo  Codigo      codmat  X(6)    Codigo Articulo Codigo Articulo     Factor  >>,>>9.9999 Factor  Factor  0   Factor  FchPed  99/99/9999  Fecha   Fch.Pedido  today   FlgEst  X(1)    FlgEst  P   ImpDto  >,>>>,>>9.9999  Importe Descuento   Importe!Descuento   0   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   ImpIgv  >,>>>,>>9.9999  Importe Igv Importe Igv 0   ImpIsc  >,>>>,>>9.9999  Importe Isc Importe Isc 0   ImpLin  ->>,>>>,>>9.99  Importe Importe 0   MrgUti  ->>,>>9.99  Margen de Utilidad  Margen de! Utilidad 0   NroItm  >>9 No.Item No.Item 0   NroPed  X(12)   No. Pedido  Numero!Pedido       Pesmat  ->>,>>9.9999    Peso    Peso    0   PorDto  >>9.99  % Dscto.    % Dscto.    0   PorDto2 >>9.99  % Dscto.    % Dscto.    0   PreBas  >,>>>,>>9.9999  Precio Base Precio Base 0   PreUni  >,>>>,>>9.99999 Precio Unitario Precio Unitario 0   UndVta  x(8)    Unidad  Und     Unidad de movimiento    DesMat  X(45)   Descripci�n Descripci�n     Descripci�n del material    DesMar  X(30)   Des.Marca   Descripcion!Marca       TipVta  X(1)    Tipo Venta  Tipo venta      canate  >,>>>,>>9.9999  Cantidad    Cantidad    0   Cantidad atendida   Por_Dsctos1 ->>9.99 % Dscto % Dscto 0   Por_Dsctos2 ->>9.99 % Dscto % Dscto 0   Por_Dsctos3 ->>9.99 % Dscto % Dscto 0   Libre_c05   x(60)   Libre_c05       RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  & 6�  ���(������    00000   �P                                  �        �        �                �     i     i     i    # 	% 	& 	    �  �  �  �  �  �  �  �  �  �  �  �  �  �          "  )  0  7  ?  F  M  T  [  b  i  p  |  �  �  �  �  �  �  �  �    ��                            ����                            !    p�                    �E    undefined                                                               �       t�  �   l   ��  ��                    �����               0�J                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                     3   ����       $      H  ���                       8      
                       � ߱        �  �   "   D       �     >          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   �3                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  f  i  L              \s�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  k  q  �              �/=                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  s  t  p              , ;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  v  y  p              �;                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  {  }  �              �;                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                    �  �	              �d=                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  �  �  H              T�=                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  �  �  T               ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  �  �  T              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  �  �  |              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  �  �  |              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  �  �  |              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              ��O                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              ��b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              <�b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              ��/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              (�/                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              ��/                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              xC'                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              ��;                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              ��M                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              ��2                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              DCY                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              (2�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              <�-                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              ��R                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              �[\                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'              `�U                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              P�R                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                       �)              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                    	  @+              �f                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                      �,              x��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                      �-              0�Q                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                       �/              �m7                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                  "  %  �0              ��2                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                  '  (  �1              (%�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                  *  ,  �2              �%�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     P       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 W       CHARACTER,  canNavigate �3      �3      (4    a       LOGICAL,    closeQuery  4      44      `4   
 m       LOGICAL,    columnProps @4      l4      �4    x       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    �       CHARACTER,  hasForeignKeyChanged    88      d8      �8    �       LOGICAL,    openDataQuery   |8      �8      �8          LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	       LOGICAL,    prepareQuery    9      49      d9    &      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    3      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 @      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 J      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 T      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    ^      CHARACTER,  assignDBRow                             <  �;      ��                      <              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                      L=              L�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                      �>              hU�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                     "  �?              4=                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                  $  &   A              �=                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                  (  )  PB              �ك                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                  +  ,  PC              �ڃ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                  .  /  PD              �ۃ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                  1  2  PE              �ރ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                  4  5  PF              (z7                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  7  9  \G              �z7                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  ;  <  �H              �7                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  >  @  �I              ��4                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  B  C  �J              �4                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  E  F  �K              0�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  H  K  �L              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M          LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP    �      CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P    �      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q           CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !        CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  %      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  4      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  C      CHARACTER,  getForeignValues    @R      lR      �R  %  R      CHARACTER,  getQueryPosition    �R      �R      �R  &  c      CHARACTER,  getQuerySort    �R      �R      S  '  t      CHARACTER,  getQueryString  �R      (S      XS  (  �      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /  �      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0        CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1        CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2         LOGICAL,    removeQuerySelection    �W      �W      (X  3  1      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  F      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 T      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  _      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  n      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8        LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              TU\                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              �7�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]              ,:�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              �=�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              (>�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              ��}                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b              ��}                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              ��}                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �     Hd              X�}                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  �      CHARACTER,  getASBound  �d      e      0e  = 
 �      LOGICAL,    getAsDivision   e      <e      le  >  �      CHARACTER,  getASHandle Le      xe      �e  ?  �      HANDLE, getASHasStarted �e      �e      �e  @  �      LOGICAL,    getASInfo   �e      �e      f  A 	 �      CHARACTER,  getASInitializeOnRun    �e       f      Xf  B        LOGICAL,    getASUsePrompt  8f      df      �f  C        LOGICAL,    getServerFileName   tf      �f      �f  D  ,      CHARACTER,  getServerOperatingMode  �f      �f      g  E  >      CHARACTER,  runServerProcedure  �f      $g      Xg  F  U      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  h      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  v      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              ,�l                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              x&                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              t�L                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              ��X                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              43                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              D�X                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              ��X                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              �O                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              �O                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              �O                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              4O                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              �O                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              �Q                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z              (�O                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �     �{              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                      �|              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                      x~              �1                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                      �              ��+                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                      ��              T�+                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                      L�              ��z                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                      t�              X�z                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 5      LOGICAL,    assignLinkProperty  ؃      �      8�  P  @      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  S      CHARACTER,  getChildDataKey ��      ̄      ��  R  a      CHARACTER,  getContainerHandle  ܄      �      <�  S  q      HANDLE, getContainerHidden  �      D�      x�  T  �      LOGICAL,    getContainerSource  X�      ��      ��  U  �      HANDLE, getContainerSourceEvents    ��      ��      ��  V  �      CHARACTER,  getContainerType    ܅      �      <�  W  �      CHARACTER,  getDataLinksEnabled �      H�      |�  X  �      LOGICAL,    getDataSource   \�      ��      ��  Y  �      HANDLE, getDataSourceEvents ��      ��      �  Z  �      CHARACTER,  getDataSourceNames  Ԇ       �      4�  [  
      CHARACTER,  getDataTarget   �      @�      p�  \        CHARACTER,  getDataTargetEvents P�      |�      ��  ]  +      CHARACTER,  getDBAware  ��      ��      �  ^ 
 ?      LOGICAL,    getDesignDataObject ȇ      �      (�  _  J      CHARACTER,  getDynamicObject    �      4�      h�  `  ^      LOGICAL,    getInstanceProperties   H�      t�      ��  a  o      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  �      CHARACTER,  getObjectHidden �      <�      l�  d  �      LOGICAL,    getObjectInitialized    L�      x�      ��  e  �      LOGICAL,    getObjectName   ��      ��      �  f  �      CHARACTER,  getObjectPage   ̉      ��      (�  g  �      INTEGER,    getObjectParent �      4�      d�  h  �      HANDLE, getObjectVersion    D�      l�      ��  i  �      CHARACTER,  getObjectVersionNumber  ��      ��      �  j        CHARACTER,  getParentDataKey    Ċ      ��      $�  k  %      CHARACTER,  getPassThroughLinks �      0�      d�  l  6      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  J      CHARACTER,  getPhysicalVersion  ��      ��      �  n  `      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  s      CHARACTER,  getQueryObject  �      4�      d�  p  �      LOGICAL,    getRunAttribute D�      p�      ��  q  �      CHARACTER,  getSupportedLinks   ��      ��      ��  r  �      CHARACTER,  getTranslatableProperties   ��      �      (�  s  �      CHARACTER,  getUIBMode  �      4�      `�  t 
 �      CHARACTER,  getUserProperty @�      l�      ��  u  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w   	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  %	      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  3	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  @	      CHARACTER,  setChildDataKey 4�      `�      ��  }  O	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  _	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    r	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  �	      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 	
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  (
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  9
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  O
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  d
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  v
      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  �
      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �  �
      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �        LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �        LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 /      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  :      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  J      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 V      CHARACTER,INPUT pcName CHARACTER    l�    1  ��  0�      �       4   �����                 @�                      ��                  2  _                  ��                       2  Ě        3  \�  ؛      �       4   �����                 �                      ��                  4  ^                  ��                       4  l�  �    K  �  ��      �       4   �����                 ��                      ��                  W  Y                  |�                       W  �         X                                  ,     
                    � ߱        �  $  [  ��  ���                           $  ]  @�  ���                       x                         � ߱        x�    c  ��  �      �      4   �����                �                      ��                  d  (	                  0�                       d  ��  H�  o   g      ,                                 ��  $   h  t�  ���                       �  @         �              � ߱        ��  �   i        Ȟ  �   j  �      ܞ  �   l        �  �   n  x      �  �   p  �      �  �   r  `      ,�  �   s  �      @�  �   t        T�  �   w  �      h�  �   y         |�  �   z  |      ��  �   |  �      ��  �   }  t      ��  �   ~  �      ̟  �     ,      ��  �   �  �      ��  �   �  �      �  �   �  P	      �  �   �  �	      0�  �   �   
      D�  �   �  t
      X�  �   �  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  O	  }	  ��              Ԩ>                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ c	  آ  ���                           O   {	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  ^                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  $
                  ̭>                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    T
  T�  Ц      x      4   ����x                �                      ��                  U
  �
                  4�                       U
  d�  ��  �   W
  �      �  �   X
  T      �  �   Y
  �      0�  �   Z
  D      D�  �   [
  �      X�  �   \
  �      l�  �   ^
  p      ��  �   _
  �      ��  �   `
  X      ��  �   a
  �      ��  �   b
  �      Ч  �   c
  D       �  �   d
  �       ��  �   e
  �       �  �   f
  x!       �  �   g
  �!      4�  �   h
  h"      H�  �   i
  �"      \�  �   j
  `#      p�  �   k
  �#      ��  �   l
  X$      ��  �   m
  �$      ��  �   n
  �$      ��  �   o
  L%      Ԩ  �   p
  �%      �  �   q
  <&      ��  �   r
  �&      �  �   s
  4'      $�  �   t
  �'      8�  �   u
  ,(      L�  �   v
  h(      `�  �   x
  �(      t�  �   y
  X)      ��  �   z
  �)      ��  �   {
  *      ��  �   |
  �*      ĩ  �   }
  �*      ة  �   ~
  l+      �  �   
  �+       �  �   �
  \,      �  �   �
  �,      (�  �   �
  L-      <�  �   �
  �-      P�  �   �
  <.      d�  �   �
  �.      x�  �   �
  4/      ��  �   �
  �/          �   �
  $0      d�      ��  8�      T0      4   ����T0                H�                      ��                    �                  ��                         ̪  \�  �     �0      p�  �     (1      ��  �     �1      ��  �   	  2      ��  �   
  �2      ��  �     3      ԫ  �     |3      �  �     �3      ��  �     t4      �  �     �4      $�  �     l5      8�  �     �5      L�  �     d6      `�  �     �6      t�  �     L7      ��  �     �7      ��  �     <8      ��  �     �8      Ĭ  �     ,9      ج  �     �9      �  �     :       �  �     X:      �  �     �:      (�  �     H;      <�  �     �;      P�  �     8<          �      �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  @                  T�g                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
        b                  ,�Y                          <�  ̱  �     �K      $�  $    ��  ���                       �K     
                    � ߱        8�  �     L      ��  $     d�  ���                       ,L  @         L              � ߱        L�  $    ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V     �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $  1  x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   C  �  ���                                      ̵                      ��                  d                    ��Y                       d  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   y  �  ���                        adm-clone-props �  ��              �     W     `                          \  `                     start-super-proc    �  d�  �           �     X                                  �                     l�      �   �      �X      4   �����X      /     ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $  4  ��  ���                       @Y                         � ߱        ��    D  �  \�  ��  \Y      4   ����\Y                и                      ��                  E  I                  ��Y                       E  �  pY                     �Y                     �Y                         � ߱            $  F  l�  ���                             J  �  T�      �Y      4   �����Y  �Y                         � ߱            $  K  (�  ���                       �Y                         � ߱        ع  $  O  ��  ���                       Ժ    R  ��  �  \�  �Y      4   �����Y      $  S  0�  ���                       Z                         � ߱            �   p  ,Z      lZ     
                �Z                     8\  @        
 �[              � ߱         �  V   �  p�  ���                        �  �   �  D\      �    6  0�  @�      �\      4   �����\      /   7  l�     |�                          3   �����\            ��                      3   �����\  �\     
                P]                     �^  @        
 `^              � ߱        ��  V   C  ��  ���                        �^     
                h_                     �`  @        
 x`              � ߱        ̼  V   g  <�  ���                        L�    �  �  d�      �`      4   �����`                t�                      ��                  �  �                  ��6                       �  ��  �  /   �  ��     ��                          3   �����`            н                      3   �����`      /   �  �     �                          3   ����a            <�                      3   ����8a  ��  /  T  x�         la                      3   ����Ta  initProps   x�  ��              �     Y     �                          �  4  	                                   ̿          t�  \�      ��                 ,  E  ��              �Z�                    O   ����    e�          O   ����    R�          O   ����    ��      >                      ��          ��  p   7  �v  �      B  t�  d�     �v                                        ��                  8  T                  �`l                       8  ��   �  ��     �v                                        ��                  U  q                  �al                       U  ��  ��  |�     �v                                        ��                  r  �                  xbl                       r  �  �  �     �v                                        ��                  �  �                  Hcl                       �  ��  ��  ��     �v                                        ��                  �  �                  dl                       �  (�  0�   �     �v                                        ��                  �  �                  lel                       �  ��  ��  ��     w                                        ��                  �                    �el                       �  @�  H�  8�     w                                        ��                                      �fl                         ��  ��  ��     0w  	                                      ��             	        <                  �gl                          X�  `�  P�     Dw  
                                      ��             
     =  Y                  Xhl                       =  ��  ��  ��     Xw                                        ��                  Z  v                  �|                       Z  p�  x�  h�     lw                                        ��                  w  �                  ��|                       w  ��  �  ��     �w                                        ��                  �  �                  ��|                       �  ��  ��  ��     �w                                        ��                  �  �                  |�|                       �  �  �  �     �w                                        ��                  �  �                  L�|                       �  ��  ��  ��     �w                                        ��                  �                    L�|                       �  ,�  4�  $�     �w                                        ��                    $                  �|                         ��      ��     �w                                        ��                  %  A                  ��|                       %  D�      O   D  ��  ��  �w               D�          ,�  8�   , �                                                       �     ��                            ����                            ��  d�  ��  �      ��     Z     L�                      � H�  P                     ��    Z  �  ��      x      4   ����x                ��                      ��                  [  o                  <�|                       [  �  ��  /   \  ��     ��                          3   ����x            ��                      3   ����4x  h�  /   ]  (�     8�                          3   ����Lx            X�                      3   ����lx  ��  /   b  ��     ��                          3   �����x            ��                      3   �����x      /   h   �     �                          3   �����x            0�                      3   �����x  y     
                �y                     �z  @        
 �z              � ߱        ��  V   �  @�  ���                        ��  $  �  ��  ���                       �z                         � ߱        {     
                �{                     �|  @        
 �|              � ߱        ��  V   �  (�  ���                        t�  $    ��  ���                       �|     
                    � ߱        �|     
                x}                     �~  @        
 �~              � ߱        ��  V   "  �  ���                        \�  $  =  ��  ���                       �~     
                    � ߱        �~     
                d                     ��  @        
 t�              � ߱        ��  V   G  ��  ���                        D�  $  a  ��  ���                       ̀                         � ߱        �     
                p�                     ��  @        
 ��              � ߱        p�  V   k  ��  ���                        ��  �   �  ؂      @�  $  �  ��  ���                       ��     
                    � ߱        �     
                ��                     ؄  @        
 ��              � ߱        l�  V   �  ��  ���                        ��  $  �  ��  ���                       �     
                    � ߱        ��  �   �  ��      0�  $  �  �  ���                       8�     
                    � ߱        D�  �   �  L�      ��  $  
  p�  ���                       ��                         � ߱                ��  ��      ��      4   ������      /     ��     �                          3   ����ȅ  4�     
   $�                      3   �����  d�        T�                      3   ������  ��        ��                      3   �����            ��                      3   ���� �  pushRowObjUpdTable  ��  ��  �                   [      �                               f                      pushTableAndValidate    ��  4�  �           |     \     �                          �  �                      remoteCommit    L�  ��  �           p     ]     �                          �  �                      serverCommit    ��  �  �           l     ^     �                          �  �                                      4�          �  ��      ��                  9  F  �              4�.                    O   ����    e�          O   ����    R�          O   ����    ��          O   D  ��  ��  P�    ��                            ����                            $�  P�      ��              _      L�                      
�     �                      disable_UI  ��  ��                      `      �                               �   
                    �  �    ����  �       ��       ��  8   ����   ��  8   ����       8   ����       8   ����       ��  ��      viewObject  ,   ��  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  �  4�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  �  x�  ��      returnFocus ,INPUT hTarget HANDLE   h�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  ��  �      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  \�  l�      removeAllLinks  ,   L�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE p�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  t�  ��      hideObject  ,   d�  ��  ��      exitObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  ��  ��      displayLinks    ,   ��  �  �      createControls  ,   ��  (�  8�      changeCursor    ,INPUT pcCursor CHARACTER   �  d�  p�      applyEntry  ,INPUT pcField CHARACTER    T�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  �  �      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  h�  p�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE X�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  �      runServerObject ,INPUT phAppService HANDLE  ��  8�  L�      disconnectObject    ,   (�  `�  p�      destroyObject   ,   P�  ��  ��      bindServer  ,   t�  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  ��  �      startFilter ,   ��  �  (�      releaseDBRow    ,   �  <�  L�      refetchDBRow    ,INPUT phRowObjUpd HANDLE   ,�  x�  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE h�  ��  ��      fetchDBRowForUpdate ,   ��  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  (�  8�      compareDBRow    ,   �  L�  `�      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   <�  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  �  �      updateState ,INPUT pcState CHARACTER    ��  D�  X�      updateQueryPosition ,   4�  l�  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    \�  ��  ��      undoTransaction ,   ��  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  x�  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   h�  ��  �      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  P�  d�      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  @�  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  ,�  @�      startServerObject   ,   �  T�  d�      setPropertyList ,INPUT pcProperties CHARACTER   D�  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ��   �      rowObjectState  ,INPUT pcState CHARACTER    ��  ,�  <�      retrieveFilter  ,   �  P�  d�      restartServerObject ,   @�  x�  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   h�  ��  ��      refreshRow  ,   p�  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  �  $�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  �  T�  l�      initializeServerObject  ,   D�  ��  ��      initializeObject    ,   p�  ��  ��      home    ,   ��  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��   �  �      fetchPrev   ,   ��   �  ,�      fetchNext   ,   �  @�  L�      fetchLast   ,   0�  `�  l�      fetchFirst  ,   P�  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   p�  ��  ��      endClientDataRequest    ,   ��  ��  ��      destroyServerObject ,   ��  �  �      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    ��  h�  x�      dataAvailable   ,INPUT pcRelative CHARACTER X�  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  ��  �      commitTransaction   ,   ��   �  0�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    �  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 %     adecomm/as-utils.w  
"   
   �    }        �
"     
    �     }        ��    ?   %               � 
"    
 � %              h �P  \         (          
�                          
�            � `   n
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 =�               1� p  
 =� {   � %               o%   o           � �    =
"   
 =�           �    1� �   =� {   � %               o%   o           � �   =
"   
 =�           �    1� �  
 =� {   � %               o%   o           � �   =
"   
 =�           l    1� �   =� {   � %               o%   o           � �    =
"   
 =�           �    1� �   =� {   � %               o%   o           � �   =
"   
 =�           T    1� �   =� �   � %               o%   o           %               
"   
 � �          �    1� �   � �      
"   
 =�               1�    =� {   � %               o%   o           � !  =
"   
 =�           �    1� #   =� {   � %               o%   o           � 2  S =
"   
 =�           �    1� �   =� �   � %               o%   o           %               
"   
 =�           p    1� �   =� �   � %               o%   o           %               
"   
 =�           �    1� �   =� �   � %               o%   o           %              
"   
 � �          h    1� �   � � �     
"   
 =�           �    1� �  
 =� �   � %               o%   o           %               
"   
 =�                1� �   =� {   � %               o%   o           � �    =
"   
 � �          �    1� �   � �      
"   
 =�           �    1� �   =� {   � %               o%   o           � �  t =
"   
 � �          D	    1� r  
 � �      
"   
 =�           �	    1� }   =� {   � %               o%   o           � �  � =
"   
 =�           �	    1�    =� {   � %               o%   o           � �    =
"   
 =�           h
    1� 2  
 =� =   � %               o%   o           %               
"   
 >�           �
    1� A   >� �   � %               o%   o           %              
"   
 ��           `    1� I   �� {   � %               o%   o           � �    >
"   
 ��           �    1� Z   �� {   � %               o%   o           o%   o           
"   
 C�           P    1� j  
 C� {   � %               o%   o           � �    l
"   
 ��           �    1� u   �� �  	 � %               o%   o           � �  / C
"   
 � �          8    1� �   � � �  	   
"   
 l�           t    1� �   l� �  	 � o%   o           o%   o           � �    l
"   
 � �          �    1� �   � � �  	   
"   
 l�           $    1� �   l� �  	 � o%   o           o%   o           � �    l
"   
 � �          �    1�    � � �     
"   
 � �          �    1�    � � �  	   
"   
 � �              1�    � � �  	   
"   
 � �          L    1� ,   � � �  	   
"   
 ��           �    1� :   �� �   � o%   o           o%   o           %              
"   
 � �              1� K   � � �  	   
"   
 � �          @    1� Y  
 � � d     
"   
 � �          |    1� l   � � �  	   
"   
 � �          �    1� {   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          0    1� �   � � �  	   
"   
 � �          l    1� �  	 � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 ��                1� �   �� {   � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
 n(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�            �� �     p�               �L
�    %              � 8          � $         �           
�    �      
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1�   
 � {   � %               o%   o           � �    
"   
 �           <    1� *  
 � {   � %               o%   o           o%   o           
"   
 ?�           �    1� 5   ?�    � %               o%   o           o%   o           
"   
 ��           4    1� >   �� �   � %               o%   o           %               
"   
 >�           �    1� M   >� �   � %               o%   o           %               
"   
 �           ,    1� Z   � {   � %               o%   o           � �    >
"   
 ��           �    1� a   �� �   � %               o%   o           %              
"   
 ��               1� s   �� �   � %               o%   o           o%   o           
"   
 C�           �    1�    C� {   � %               o%   o           o%   o           
"   
 ?�               1� �  	 ?� {   � %               o%   o           � �    l
"   
 ?�           �    1� �   ?� {   � %               o%   o           o%   o           
"   
 ?�               1� �   ?� {   � %               o%   o           o%   o           
"   
 >�           �    1� �   >� �   � %               o%   o           %               
"   
 >�           �    1� �   >� �   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� �  
 � �   � %               o%   o           %              
"   
 �           H    1� �   � {   � %               o%   o           o%   o           
"   
 �           �    1� �   � {   � %               o%   o           � �    C
"   
 �           8    1� �   � {   � %               o%   o           o%   o           
"   
 � �          �    1�    � �      
"   
 ��           �    1�    �� {   � %               o%   o           � '  ! ?
"   
 ��           d    1� I   �� {   � %               o%   o           � �    �
"   
 l�           �    1� V   l� {   � %               o%   o           � i   �
"   
 � �          L    1� x   � � �     
"   
 � �          �    1� �   � �      
"   
 >�           �    1� �   >� {   � %               o%   o           � �    ?
"   
 � �          8     1� �  
 � �      
"   
 �           t     1� �   � �   � %               o%   o           o%   o           
"   
 ��           �     1� �   �� �   � %               o%   o           %               
"   
 ?�           l!    1� �   ?� �   � %               o%   o           %               
"   
 C�           �!    1� �   C� {   � %               o%   o           � �    ?
"   
 C�           \"    1� �   C� {   � %               o%   o           o%   o           
"   
 �           �"    1� �   � �   � %               o%   o           %              
"   
 ��           T#    1�    �� �   � %               o%   o           %               
"   
 �           �#    1�    � �   � %               o%   o           %               
"   
 � �          L$    1� ,   � �      
"   
 � �          �$    1� 9   � � {     
"   
 ��           �$    1� F   �� =   � %               o%   o           o%   o           
"   
 C�           @%    1� R   C� {   � %               o%   o           � �    l
"   
 C�           �%    1� `   C� {   � %               o%   o           o%   o           
"   
 �           0&    1� n   � �   � o%   o           o%   o           o%   o           
"   
 �           �&    1� �   � �  	 � %               o%   o           o%   o           
"   
 �           ('    1� �   � {   � %               o%   o           o%   o           
"   
 ?�           �'    1� �  
 ?� =   � %               o%   o           o%   o           
"   
 � �           (    1� �   � � {     
"   
 ��           \(    1� �   �� {   � %               o%   o           � �  4 ?
"   
 ��           �(    1� 	  
 �� �   � %               o%   o           %              
"   
 � �          L)    1�    � �      
"   
 ?�           �)    1� %   ?� {   � %               o%   o           � �    
"   
 l�           �)    1� 3   l� �   � %               o%   o           %              
"   
 C�           x*    1� B   C� {   � %               o%   o           � �    l
"   
 >�           �*    1� O   >� {   � %               o%   o           � �    C
"   
 ?�           `+    1� ]   ?� {   � %               o%   o           � �    >
"   
 �           �+    1� i   � �   � %               o%   o           %               
"   
 �           P,    1� x  	 �    � %               o%   o           o%   o           
"   
 �           �,    1� �   � {   � %               o%   o           � �  	 �
"   
 ?�           @-    1� �   ?� =   � %               o%   o           %       �       
"   
 l�           �-    1� �   l� {   � %               o%   o           � �    ?
"   
 l�           0.    1� �   l� �   � o%   o           o%   o           %              
"   
 >�           �.    1� �   >� �   � %               o%   o           %               
"   
 >�           (/    1� �   >� {   � %               o%   o           o%   o           
"   
 �           �/    1� �   � �  	 � %               o%   o           � �    
"   
 � �          0    1� �   � � �  	   P �L 
�H T   %              �     }        �GG %              
"   
 �           �0    1�   
 � {   � %               o%   o           � �    
"   
 ?�           1    1�    ?� �   � %               o%   o           %               
"   
 l�           �1    1�   	 l� {   � %               o%   o           � �    ?
"   
 ��           2    1� (   �� {   � %               o%   o           � �    l
"   
 >�           �2    1� 6   >� �   � %               o%   o           %               
"   
 ?�           �2    1� F   ?� {   � %               o%   o           � �    >
"   
 ?�           p3    1� Y   ?� {   � %               o%   o           o%   o           
"   
 �           �3    1� a   � {   � %               o%   o           o%   o           
"   
 ��           h4    1� n   �� �   � %               o%   o           o%   o           
"   
 �           �4    1� |   � �   � %               o%   o           o%   o           
"   
 ?�           `5    1� �   ?� �   � %               o%   o           o%   o           
"   
 l�           �5    1� �   l� {   � %               o%   o           o%   o           
"   
 ?�           X6    1� �  	 ?� �  	 � %               o%   o           � �    �
"   
 C�           �6    1� �  
 C� �  	 � %               o%   o           � �    ?
"   
 �           @7    1� �   � {   � %               o%   o           � �    C
"   
 �           �7    1� �   � {   � %               o%   o           o%   o           
"   
 >�           08    1� �   >� {   � %               o%   o           o%   o           
"   
 �           �8    1� �   � {   � %               o%   o           � �    ?
"   
 l�            9    1�     l� {   � %               o%   o           � �    
"   
 l�           �9    1�    l� �  	 � %               o%   o           o%   o           
"   
 � �          :    1� !   � �      
"   
 ��           L:    1� -   �� {   � %               o%   o           � �    ?
"   
 ��           �:    1� ;   �� {   � %               o%   o           o%   o           
"   
 �           <;    1� N   � �   � %               o%   o           o%   o           
"   
 ?�           �;    1� `  
 ?� {   � %               o%   o           � �    >
"   
 ?�           ,<    1� k   ?� {   � %               o%   o           � �    ?
"   
 C�           �<    1� �   C� �   � %               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 C�           p=    1� �  	 C�    � %               o%   o           o%   o           
"   
 ��           �=    1� �   ��    � %               o%   o           o%   o           
"   
 �           h>    1� �   �    � %               o%   o           o%   o           
"   
 �           �>    1� �   � �   � %               o%   o           %              
"   
 >�           `?    1� �   >� {   � %               o%   o           � �  M 
"   
 ?�           �?    1� 7   ?� �   � %               o%   o           %              
"   
 ?�           P@    1� H   ?� �   � %               o%   o           %               
"   
 l�           �@    1� \   l� �   � %               o%   o           %               
"   
 ?�           HA    1� s   ?� �  	 � %               o%   o           � �   l
"   
 �           �A    1� �   � �   � %               o%   o           %               
"   
 �           8B    1� �   � �  	 � %               o%   o           o%   o           
"   
 �           �B    1� �   � �   � o%   o           o%   o           %              
"   
 �           0C    1� �   � �  	 � o%   o           o%   o           � �    
"   
 >�           �C    1� �   >�    � o%   o           o%   o           o%   o           
"   
 >�            D    1� �   >�    � o%   o           o%   o           o%   o           
"   
 >�           �D    1� �   >� �  	 � o%   o           o%   o           o%   o           
"   
 >�           E    1�    >�    � o%   o           o%   o           o%   o           
"   
 >�           �E    1�    >� �  	 � o%   o           o%   o           � (   >
"   
 ?�           F    1� *   ?� �  	 � o%   o           o%   o           � 9   ?
"   
 l�           |F    1� E   l� �   � %               o%   o           %               
"   
 ��           �F    1� Y   �� �   � %               o%   o           %               
"   
 � �          tG    1� m   � � �  	   
"   
 ��           �G    1� �   �� �   � %               o%   o           %               
"   
 ��           ,H    1� �   �� {   � %               o%   o           o%   o           
"   
 ?�           �H    1� �   ?� {   � %               o%   o           o%   o           
"   
 �           $I    1� �   � �   � %               o%   o           o%   o           
"   
 ?�           �I    1� �   ?� {   � %               o%   o           � �    ?
"   
 �           J    1� �   � �   � %               o%   o           %               
"   
 ��           �J    1� �  	 �� �   � %               o%   o           %                "    � %     start-super-proc � %     adm2/smart.p nP �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6� �     
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 n
"   
 � 
"   
 n
"   
   (�  L ( l       �        �M    �� �   � P   �        �M    �@    
� @  , 
�       �M    �� �   np�               �L
�    %              � 8       N    � $         �           
�    �    n
"   
 �p� @  , 
�       O    ��    �p�               �L"  	  , �   � "   ?� $   � �     }        �A      |    "  	    � "   %              (<   \ (    |    �     }        �A� &   �A"  
  ?    "  	  n"  
  ?  < "  	  n"  
  ?(    |    �     }        �A� &   �A"  
  ?
�H T   %              �     }        �GG %              
"   
 n
"   
 � 
"   
 n
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   np�               �L
�    %              � 8      Q    � $         �           
�    �    n
"   
 �p� @  , 
�       R    �� p  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 D
"   
   
"   
   (�  L ( l       �        �R    �� �   � P   �        �R    �@    
� @  , 
�       �R    �� �     p�               �L
�    %              � 8      �R    � $         �           
�    �      
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� �     p�               �L%               
"   
  p� @  , 
�       �T    �� �    p�               �L%               
"   
  p� @  , 
�       U    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 ? (   � 
"   
 n    �        �U    �� �   �
"   
   � 8      DV    � $         �           
�    �    n
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6� �     
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � O   
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 n    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 ?"      �       }        �
"   
 � %              %                "    � %     start-super-proc � %     adm2/appserver.p ��    � �     
�    �     }        �%               %      Server  - �     }        �    "    �� �    � %               %      Client      "    �� �    � %      NONE    p�,  8         $     "    ?        � �   n
�    
�H T   %              �     }        �GG %              
"   
 n
"   
 � 
"   
 n
"   
   (�  L ( l       �        �Z    �� �   � P   �        �Z    �@    
� @  , 
�       �Z    �� �   np�               �L
�    %              � 8      �Z    � $         �           
�    �    n
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "    ?        � �   n
�     "    � %     start-super-proc � %     adm2/dataquery.p �l
�H T   %              �     }        �GG %              
"   
 n
"   
 � 
"   
 n
"   
 n(�  L ( l       �         ]    �� �   � P   �        ,]    �@    
� @  , 
�       8]    �� �   np�               �L
�    %              � 8      D]    � $         �    n     
�    �    n
"   
 �p� @  , 
�       T^    �� }   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 n
"   
 � 
"   
 n
"   
 n(�  L ( l       �        8_    �� �   � P   �        D_    �@    
� @  , 
�       P_    �� �   np�               �L
�    %              � 8      \_    � $         �    n     
�    �    n
"   
 �p� @  , 
�       l`    �� �   �p�               �L%               "    � %     start-super-proc � %     adm2/query.p n%     start-super-proc � %     adm2/queryext.p % 	    initProps n
�    %` U P   FOR EACH FacDPedi NO-LOCK,       EACH Almmmatg OF FacDPedi NO-LOCK INDEXED-REPOSITION n�   � �     � �     � �     
�H T   %              �     }        �GG %              
"   
 n
"   
 � 
"   
 n
"   
   (�  L ( l       �        Pb    �� �   � P   �        \b    �@    
� @  , 
�       hb    �� �   np�               �L
�    %              � 8      tb    � $         �           
�    �    n
"   
 �p� @  , 
�       �c    �� �   �p�               �L"    ,     "    l� �    � 
�H T   %              �     }        �GG %              
"   
 n
"   
 � 
"   
 n
"   
   (�  L ( l       �        Hd    �� �   � P   �        Td    �@    
� @  , 
�       `d    �� �   np�               �L
�    %              � 8      ld    � $         �           
�    �    n
"   
 �p� @  , 
�       |e    ��   	 �p�               �L"    , %� � �   rowObject.Por_Dsctos1 = FacDPedi.Por_Dsctos[1]  rowObject.Por_Dsctos2 = FacDPedi.Por_Dsctos[2]  rowObject.Por_Dsctos3 = FacDPedi.Por_Dsctos[3]  �    "      � �         %              %                   "      %                  "      "      "     T(        "    (%              "    (� �   � "      �       "    n�    "    (� &   � � �      � &   n�    "     � &    S    "      "    �     "    6%                � @    �     t T     P   4       � "      (0       4       O"      � �      � �    n� �   OT ,  %              T   "    O"    � � �     � &   n� �   OT    �    "    O� &   � "      � &   n"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    n%              � �    � � <     4  O     "      
�H T   %              �     }        �GG %              
"   
 n
"   
 � 
"   
 n
"   
   (�  L ( l       �        �j    �� �   � P   �        �j    �@    
� @  , 
�       �j    �� �   np�               �L
�    %              � 8      �j    � $         �           
�    �    n
"   
 �p� @  , 
�        l    ��   
 �p�               �L"    ,       "  
  �    � >  � �� �   �       "  	    �    � >  � � � �   ��   � �     � �     � >  � n�   � �     � �   n� >  � �      "  
  D�    � �    D� �   �       "  	    �    � +   � � �   D   ,        "    n� <   D�   � �   n� �   D� �    �    ,        "      � <     �   � �   D� �   � � +   D�   � �     � �     � 9  �   
�H T   %              �     }        �GG %              
"   
 � 
"   
 n
"   
 � 
"   
 � (�  L ( l       �        �n    �� �   � P   �        �n    �@    
� @  , 
�       �n    �� �   � p�               �L
�    %              � 8      �n    � $         �           
�    �      
"   
 �p� @  , 
�       �o    �� �   �p�               �L"    , 
"   
   p� @  , 
�       ,p    �� k     p�               �L"    , 
"   
  p� @  , 
�       �p    �� F    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � >  �   � �         "  	  O�     "    6T    "      "      @ A,    �   � �   � � <     "    n"       T      @   "    � (        "      � �    n� �      � �   n"    D     "  	   %              D H   @ A,    �   � �   n� <     "    n"    ?,    S   "    n� >  � ?� �   � %                T      @   "    � (        "      � �    n� �      � �   n"    l     "  
   %                         "    � � <     "    n           "      � <   n"      
�H T   %              �     }        �GG %              
"   
 O
"   
   
"   
 O
"   
 n(�  L ( l       �        �t    �� �   � P   �        �t    �@    
� @  , 
�       �t    �� �   Op�               �L
�    %              � 8      �t    � $         �    n     
�    �    � 
"   
 �p� @  , 
�       �u    �� k   �p�               �L"    , 
"   
   p� @  , 
�       ,v    �� F     p�               �L"    , "      %              %              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    � %     start-super-proc � %     adm2/data.p %     start-super-proc � %     adm2/dataext.p %     start-super-proc � %     adm2/dataextcols.p %     start-super-proc � %     adm2/dataextapi.p D
�H T   %              �     }        �GG %              
"   
 n
"   
 � 
"   
 n
"   
 n(�  L ( l       �        Ty    �� �   � P   �        `y    �@    
� @  , 
�       ly    �� �   np�               �L
�    %              � 8      xy    � $         �    n     
�    �    n
"   
 �p� @  , 
�       �z    �� �   �p�               �L%               %$     "pruebas/dcotcreditodet.i" 
�H T   %              �     }        �GG %              
"   
 n
"   
 � 
"   
 n
"   
   (�  L ( l       �        \{    �� �   � P   �        h{    �@    
� @  , 
�       t{    �� �   np�               �L
�    %              � 8      �{    � $         �           
�    �    n
"   
 �p� @  , 
�       �|    �� s   �p�               �L"    , 
�     	         �G
�H T   %              �     }        �GG %              
"   
 n
"   
 � 
"   
 n
"   
   (�  L ( l       �        H}    �� �   � P   �        T}    �@    
� @  , 
�       `}    �� �   np�               �L
�    %              � 8      l}    � $         �           
�    �    n
"   
 �p� @  , 
�       |~    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 n
"   
 � 
"   
 n
"   
   (�  L ( l       �        4    �� �   � P   �        @    �@    
� @  , 
�       L    �� �   np�               �L
�    %              � 8      X    � $         �           
�    �    n
"   
 �p� @  , 
�       h�    �� x  	 �p�               �L
"   
 , 
"   
 �      �    	   �        ��    �
�H T   %              �     }        �GG %              
"   
 n
"   
 � 
"   
 n
"   
   (�  L ( l       �        @�    �� �   � P   �        L�    �@    
� @  , 
�       X�    �� �   np�               �L
�    %              � 8      d�    � $         �           
�    �    n
"   
 �p� @  , 
�       t�    �� �   �p�               �L"    , 
"   
   �       ̂    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 n
"   
 � 
"   
 n
"   
   (�  L ( l       �        X�    �� �   � P   �        d�    �@    
� @  , 
�       p�    �� �   np�               �L
�    %              � 8      |�    � $         �           
�    �    n
"   
 �p� @  , 
�       ��    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 �        �     n
�    
�             �Gp�,  8         $     
"   
 �        � +    n
�    �    � =      
�        "    D� �    � %     modifyListProperty  
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � �      
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   l       ��                 q  �  �               P6                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       �U     
                    � ߱              �  (  �      V      4   ����V                �                      ��                  �  �                  �Q6                       �  8  �  �  �  PV            �  �  `      �V      4   �����V                p                      ��                  �  �                  \�(                       �  �  �  o   �      ,                                 �  �   �  �V      �  �   �  �V      $  $  �  �  ���                        W     
                    � ߱        8  �   �  @W      L  �   �  `W      `  �   �  �W          $   �  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               ,�6                    O   ����    e�          O   ����    R�          O   ����    ��      p                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  ��6                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��                 `    �               ��6                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       ta                         � ߱        �  $  �  8  ���                       �a                         � ߱        b     
                �b                     �c  @        
 �c              � ߱        �  V   �  d  ���                        �    �    �      �c      4   �����c  �c     
                xd                     �e  @        
 �e              � ߱            V        ���                          $  +  �  ���                       �e                         � ߱        �  $  ,  4  ���                       pf                         � ߱          �      4  8                      ��        0         .  D                  ��.      g     �     .  `      $  .    ���                       �f                         � ߱        �  $  .  `  ���                       �f                         � ߱            4   �����f  g                     \g                     hg                     �g                     �g                         � ߱        d  $  /  �  ���                             <  �  �      �g      4   �����g      $  =  �  ���                        h          Li             � ߱        �  $  G    ���                       Xi                         � ߱          �        |                      ��        0         I  N                  ��.      �i     8     I  @      $  I  �  ���                       li                         � ߱        l  $  I  @  ���                       �i                         � ߱            4   �����i      $  K  �  ���                        j                         � ߱        �j     
                �j                     Ll  @        
 l              � ߱        �  V   Y  �  ���                        Xl       
       
       �l       	       	       �l                     �l                         � ߱        �	  $  �  d  ���                       m       
       
       Lm       	       	       �m                     �m                         � ߱        �	  $  �  	  ���                       �
  $  D  �	  ���                       (n                         � ߱        Tn     
                �n                      p  @        
 �o          xp  @        
 8p          �p  @        
 �p              � ߱        H  V   P  
  ���                          X      �  (                      ��        0         �  �                  �W�      \q           �  �
      $  �  �  ���                       �p                         � ߱          $  �  �  ���                       q                         � ߱          4   ����4q      4   ����pq  �  $  �  T  ���                       �q                         � ߱        �    �  �        �q      4   �����q                p                      ��                  �  �                  �X�                       �  �  8r                     �r       	       	           � ߱            $  �  (  ���                             �  �  4      �r      4   �����r                �                      ��                  �  �                  8Y�                       �  �  \s                     �s       
       
           � ߱            $  �  D  ���                       �s                      t                         � ߱        �  $  �  �  ���                       Tt     
                �t                      v  @        
 �u          xv  @        
 8v              � ߱            V   �  ,  ���                                    7 �          �    �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �   �   �   �   �   �   �   �       $  4  D  T  d  t  �  \  �  �  �  �  8HXhx��������(8HX      �   �   �   �   �   �   �   �      $  4  D  T  d  t  �   \ �  �  �  �  8HXhx��������(8HX  �                    � �                     �          ��                            ����                                                        �   l       ��                  �  �  �               T`N                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                  �  �  �               ldN                    O   ����    e�          O   ����    R�          O   ����    ��      y        �              �                  $                  d  /  �  $     4  X�                      3   ����<�            T                      3   ����`�      O   �  ��  ��  l�               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  �    �               �;                    O   ����    e�          O   ����    R�          O   ����    ��      �        �              �                $                  �        ,             �          �                                 �  /  �  t     �  ��                      3   ����p�            �                      3   ������     /  �  �     �  Ć                      3   ������  x                             3   ����̆      $   �  L  ���                                                   � ߱                  �  �                  3   ����؆      $   �  �  ���                                                   � ߱        X  $  �  ,  ���                       �                         � ߱            O   �  ��  ��   �               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                    ,  �               >                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  �                     �          �                       �              /  )  L     \  0�                      3   �����  �        |  �                  3   ����8�      $   )  �  ���                                                   � ߱                                      3   ����D�      $   )  @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                  �  �  �               ��.                    O   ����    e�          O   ����    R�          O   ����    ��            �  �   �       d�      4   ����d�      �   �  x�    ��                            ����                            TXS appSrvUtils .\aplic\pruebas\dcotcreditodet.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "pruebas/dcotcreditodet.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server Client NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH FacDPedi NO-LOCK,       EACH Almmmatg OF FacDPedi NO-LOCK INDEXED-REPOSITION ,   FacDPedi Almmmatg  rowObject.Por_Dsctos1 = FacDPedi.Por_Dsctos[1]  rowObject.Por_Dsctos2 = FacDPedi.Por_Dsctos[2]  rowObject.Por_Dsctos3 = FacDPedi.Por_Dsctos[3] ; AftIgv AftIsc CanPed CodCia CodCli CodDiv CodDoc codmat Factor FchPed FlgEst ImpDto ImpDto2 ImpIgv ImpIsc ImpLin MrgUti NroItm NroPed Pesmat PorDto PorDto2 PreBas PreUni UndVta TipVta canate Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 Libre_c05 DesMat DesMar AftIgv AftIsc CanPed CodCia CodCli CodDiv CodDoc codmat Factor FchPed FlgEst ImpDto ImpDto2 ImpIgv ImpIsc ImpLin MrgUti NroItm NroPed Pesmat PorDto PorDto2 PreBas PreUni UndVta DesMat DesMar TipVta canate Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 Libre_c05 INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p AftIgv AftIsc CanPed CodCia CodCli CodDiv CodDoc codmat Factor FchPed FlgEst ImpDto ImpDto2 ImpIgv ImpIsc ImpLin MrgUti NroItm NroPed Pesmat PorDto PorDto2 PreBas PreUni UndVta DesMat DesMar TipVta canate Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 Libre_c05 RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery |  2  �  �?      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   c	  {	  }	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable             |     cQueryString    |  �  #   Y   �          �                  initProps   �  �  �  �    +  ,  .  /  <  =  D  G  I  K  N  Y  �  �  D  P  �  �  �  �  �  �  �  �  �  �  �  �  �              h     lRet              �        piTableIndex    �  �  (   Z   T  p      �                  deleteRecordStatic  7  8  T  U  q  r  �  �  �  �  �  �  �  �           <  =  Y  Z  v  w  �  �  �  �  �  �  �  �      $  %  A  B  D  E                 !       �  �     [       |      �                  pushRowObjUpdTable  �           �        pcValType                  $       �  `     \       �      H                  pushTableAndValidate    �  �  �  �        �        pcContext   �             $       �        �        pcMessages            �        pcUndoIds     0     ]       l                         remoteCommit    �  �  �  �    \             $       �        t        pcMessages            �        pcUndoIds   �  �     ^       D      �                  serverCommit    )  ,  �  0     _                                 getRowObjUpdStatic  D  F  �  t     `               h                  disable_UI  �  �  8  h#       (      0#                      �  �  �  &   RowObject   �         �         �         �         �         �         �         �         �         �         �         �         �         �                                                        (         0         8         @         H         P         X         `         h         p         x         �         �         �         �         �         �         �         �         AftIgv  AftIsc  CanPed  CodCia  CodCli  CodDiv  CodDoc  codmat  Factor  FchPed  FlgEst  ImpDto  ImpDto2 ImpIgv  ImpIsc  ImpLin  MrgUti  NroItm  NroPed  Pesmat  PorDto  PorDto2 PreBas  PreUni  UndVta  DesMat  DesMar  TipVta  canate  Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 Libre_c05   RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �  �  '   RowObjUpd   �         �         �         �         �         �         �                                             $         ,         4         <         D         L         T         \         d         l         t         |         �         �         �         �         �         �         �         �         �         �         �         �         �                                     AftIgv  AftIsc  CanPed  CodCia  CodCli  CodDiv  CodDoc  codmat  Factor  FchPed  FlgEst  ImpDto  ImpDto2 ImpIgv  ImpIsc  ImpLin  MrgUti  NroItm  NroPed  Pesmat  PorDto  PorDto2 PreBas  PreUni  UndVta  DesMat  DesMar  TipVta  canate  Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 Libre_c05   RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   H          <  
   appSrvUtils p       \     xiRocketIndexLimit  �        �  
   gshAstraAppserver   �        �  
   gshSessionManager   �        �  
   gshRIManager            �  
   gshSecurityManager  4           
   gshProfileManager   `        H  
   gshRepositoryManager    �  	 	     t  
   gshTranslationManager   �  
 
     �  
   gshWebManager   �        �     gscSessionId    �        �     gsdSessionObj               
   gshFinManager   @         0   
   gshGenManager   d         T   
   gshAgnManager   �         x      gsdTempUniqueID �         �      gsdUserObj  �         �      gsdRenderTypeObj    �         �      gsdSessionScopeObj  !       !  
   ghProp  4!       (!  
   ghADMProps  X!       H!  
   ghADMPropsBuf   �!       l!     glADMLoadFromRepos  �!       �!     glADMOk �!       �!  
   ghContainer �!    	   �!     cObjectName �!    
   �!     iStart  "       "     cAppService 8"       ,"     cASDivision d"       L"     cServerOperatingMode    �"       x"     cContainerType  �"       �"     cQueryString    �"       �"  
   hRowObject  �"       �"  
   hDataQuery  #        #     cColumns              #     cDataFieldDefs  L#    X  @#  RowObject         X  \#  RowObjUpd          "   >   �   �   �   �   1  2  3  4  K  W  X  Y  [  ]  ^  _  c  d  g  h  i  j  l  n  p  r  s  t  w  y  z  |  }  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  (	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  $
  T
  U
  W
  X
  Y
  Z
  [
  \
  ^
  _
  `
  a
  b
  c
  d
  e
  f
  g
  h
  i
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
            	  
                                               �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  @  �  �  �                 1  C  b  d  y        4  D  E  F  I  J  K  O  R  S  p  �  �  6  7  C  g  �  �  �  �  �  T  Z  [  \  ]  b  h  o  �  �  �    "  =  G  a  k  �  �  �  �  �  �  �  
          ��  .\aplic\pruebas\dcotcreditodet.w �'  ��  C:\Progress\OpenEdge\src\adm2\data.i �'  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    �'  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i 0(  �� , .\aplic\pruebas\dcotcreditodet.i d(  �:   C:\Progress\OpenEdge\src\adm2\query.i    �(  z + C:\Progress\OpenEdge\src\adm2\delrecst.i �(  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  �(   ) %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   ,)  � ! C:\Progress\OpenEdge\src\adm2\dataquery.i    l)  �Z ( %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   �)  �< " C:\Progress\OpenEdge\src\adm2\appserver.i    �)  �� ' %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i    *  I� # C:\Progress\OpenEdge\src\adm2\smart.i    d*  Ds & C:\Progress\OpenEdge\gui\fn  �*  tw % %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �*  Q. $ C:\Progress\OpenEdge\gui\set  +  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i (+  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    \+  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    �+  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  �+  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i ,  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i X,   
 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    �,  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   �,  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   -  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i d-  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �-  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �-  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i  .  �j  C:\Progress\OpenEdge\gui\get T.  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    |.  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �.  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i /  Su  C:\Progress\OpenEdge\src\adm2\globals.i  8/  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i l/  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �/  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �/  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   $0  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    l0  ª  %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   �0  �� 	 C:\Progress\OpenEdge\src\adm2\qryprto.i  �0  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   $1  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i h1  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    �1  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �1  ��    !C:\newsie\on_in_co\aplic\pruebas\dcotcreditodet_cl.w        �      X2  �   N     h2  [  �     x2     �  &   �2  �   t     �2       .   �2  �        �2     �     �2  �   �     �2     �  $   �2  �   �     �2     �  $   3  �   �     3     �  $   (3  �   �     83     c  $   H3  �   a     X3     ?  $   h3  �   <     x3       $   �3  �        �3     �  $   �3  �   �     �3     �  $   �3  �   �     �3     �  -   �3  �   �     �3     �  ,   4  k   M     4  �  A      (4     '  +   84  �  $      H4     
  +   X4  �        h4     �  +   x4  �  �      �4     �  +   �4  �  �      �4     �  +   �4  �  �      �4     �  +   �4  �  �      �4     y  +   �4  �  v      5     \  +   5  �  Y      (5     ?  +   85  �  <      H5     "  +   X5  �        h5       +   x5  �        �5     �  +   �5  �  �      �5     �  +   �5  �  �      �5     �  +   �5  �  �      �5     �  +   �5  �  �      6     t  +   6  �  q      (6     W  +   86  �  T      H6     :  +   X6  �        h6     �  $   x6  �  �      �6     �  $   �6  k  �      �6     �  $   �6  j  �      �6     k  $   �6  i  j      �6     H  $   �6  _  >      7       *   7  ^        (7     �  *   87  ]  �      H7     �  *   X7  \  �      h7     �  *   x7  [  �      �7     |  *   �7  Z  {      �7     U  *   �7  Y  T      �7     .  *   �7  X  -      �7       *   �7  W        8     �  *   8  V  �      (8     �  *   88  U  �      H8     �  *   X8  T  �      h8     k  *   x8  S  j      �8     D  *   �8  R  C      �8       *   �8  Q        �8     �  *   �8  P  �      �8     �  *   �8  O  �      9     �  *   9  N  �      (9     �  *   89  @  s      H9     Q  $   X9           h9     �  $   x9    �      �9     �  $   �9  �   P      �9     �  )   �9  g   �      �9  a   �  !   �9     �  (   �9  _   �  !   �9     _  $   :  ]   ]  !   :     ;  $   (:  I   '  !   8:  �     "   H:     �  '   X:  �   �  "   h:     �  $   x:  �   �  "   �:     |  $   �:  �   z  "   �:     X  $   �:  g   >  "   �:          �:  O     "   �:  �   �  #   �:     �  &   ;  �   _  #   ;       %   (;  �   �  #   8;     �  $   H;  �   �  #   X;     �  $   h;  �   �  #   x;     �  $   �;  �   �  #   �;     q  $   �;  �   ]  #   �;     ;  $   �;  }   /  #   �;       $   �;     �  #   �;     C  "   <     �  !   <     �      (<     I     8<  �   @     H<  O   2     X<     !     h<     �     x<  �   �     �<  �   �     �<  O   �     �<     r     �<     $     �<  y   �
     �<  �   �
  
   �<  G   �
     �<     �
     =     �
     =  c   ,
  
   (=  x   $
     8=  M   
     H=     �	     X=     �	     h=  a   �	     x=  �  z	     �=     [	     �=  �  (	     �=  O   	     �=     		     �=     �     �=  �   �     �=     �     �=          >  x        >     �     (>     v     8>     r     H>     ^     X>     E     h>  Q   5     x>     �     �>     �     �>     �     �>     u     �>  ]   o  
   �>     e     �>       
   �>          �>     �  
   ?  Z   �     ?       	   (?     �     8?     �     H?     �     X?  c   y     h?     W     x?          �?     �      �?     �      �?     �      �?     &      �?           �?           