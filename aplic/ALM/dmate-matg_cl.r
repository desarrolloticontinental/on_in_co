	��V٨>f\7   �                                              � 375C00EFutf-8 MAIN D:\newsie\on_in_co\aplic\alm\dmate-matg_cl.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,codfam character 0 0,codmat character 1 0,DesMat character 2 0,DesMar character 3 0,UndStk character 4 0,StkAct decimal 5 0,CodUbi character 6 0,FchIng date 7 0,Pesmat decimal 8 0,RowNum integer 9 0,RowIdent character 10 0,RowMod character 11 0,RowIdentIdx character 12 0,RowUserProp character 13 0,ChangedFields character 14 0        �               �             R� �   �              �              D<     +   �� �  W   \� `  X   �� �  Y   <�   [   L�   \   d� <  ]   ��    ^   �� 0  `   ? � b  iSO8859-1                                                                           �     �                                      �                   ��                8   �       �   T�              ��  �   \       h                                                          PROGRESS                         �           
    
                    �              �                                                                                                     
                                                                                                            ��         �             �                                                                                             ��         @        �  
    
                  �  p             ,                                                                                                    
  �        h  
    
                  T               �                                                                                                    
  �  &        
    
                     �             �                                                                                          &          
  D  3      �  
    
                  �  t             0                                                                                          3          
  �  F      l  
    
                  X     	           �                                                                                          F          
  �  X        
    
                    �  
           �                                                                                          X          
  H  m      �  
    
                  �  x             4                                                                                          m          
  �  �      p  
    
                  \  $             �                                                                                          �          
  �  �                                 �             �                                                                                          �            L	  �      �                        �  |	             8	                                                                                          �            �	  �      t	  
    
                  `	  (
             �	                                                                                          �          
  �
  �       
  
    
                  
  �
             �
                                                                                          �          
  P  �      �
  
    
                  �
  �             <                                                                                          �          
  �  �      x                        d  ,             �                                                                                          �            �  �      $                          �             �                                                                                          �            T  �      �                        �  �             @                                                                                          �                      |                        h                �                                                                                                      h         �       �  X  �     �  �  �                �             �                �              �       �  X  8  %   `  �  f�      �         �         �    8          x      �                 P�                                               T�          X  �  L l8                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                                                                                           �  �  �  �  �          �             �       (                           ,  4  <  T  H          X             t  |  �  �  �                         �  �  �  �  �          �             �  �  �    �                         $  ,  T  @          X             l  t  �  �  �          �             �  �  �  �  �                         �  �  �  �                             �                                       (  0  8                             <  H  P  \                             `  l  t  �                                                                          codfam  X(3)    C�digo!Familia  C�digo!Familia      Codigo de familia   codmat  X(6)    Codigo!Articulo Codigo!Articulo     DesMat  X(60)   Descripci�n Descripci�n     Descripci�n del material    DesMar  X(20)   Marca   Marca       UndStk  X(8)    Unidad  Unidad      Unidad de stock StkAct  (ZZZ,ZZZ,ZZ9.99)    Stock actual    Stock actual    0   Stock actual    CodUbi  x(8)    Zona o!Ubicaci�n    Zona o!Ubicaci�n        C�digo de ubicaci�n FchIng  99/99/9999  Fecha Ing.  Fecha Ing.  ?   Fecha de Ingreso    Pesmat  ->>,>>9.9999    Peso!Kg Peso!Kg 0   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ���������       �           �        �                        �     i     i     i     	 	 	    �  �  �  �  �  �  �  �  �  �  �  �                                                                                                                                         	                  
                                                                                                                                             8  @  H  h  X          l             �  �  �  �  �                         �  �  �  �  �          �             �                                    (  0  @  8          D             T  \  p  �  �          �             �  �  �  �  �          �             �  �                 $             8  @  P  `  X                         d  l  x  �                             �  �  �  �                              �  �  �  �                             �  �  �  �                             �  �  �                                    $  4                                                                          codfam  X(3)    C�digo!Familia  C�digo!Familia      Codigo de familia   codmat  X(6)    Codigo!Articulo Codigo!Articulo     DesMat  X(60)   Descripci�n Descripci�n     Descripci�n del material    DesMar  X(20)   Marca   Marca       UndStk  X(8)    Unidad  Unidad      Unidad de stock StkAct  (ZZZ,ZZZ,ZZ9.99)    Stock actual    Stock actual    0   Stock actual    CodUbi  x(8)    Zona o!Ubicaci�n    Zona o!Ubicaci�n        C�digo de ubicaci�n FchIng  99/99/9999  Fecha Ing.  Fecha Ing.  ?   Fecha de Ingreso    Pesmat  ->>,>>9.9999    Peso!Kg Peso!Kg 0   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  �  ���������       �              �        �                        �     i     i     i     	 	 	    �  �  �  �  �  �  �  �  �  �  �  �          ��                            ����                            W    p�                    �    undefined                                                               �       t�  �   l   ��  ��                    �����               �2�                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                     3   ����       $      H  ���                       8      
                       � ߱        �  �   "   D       �     >          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   �a�                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  ]  `  L              �c�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  b  h  �              |ٱ                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  j  k  p              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  m  p  p              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  r  t  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  v  y  �	              H��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  {  |  H              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  ~    T              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  �  �  T              |��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  �  �  |              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  �  �  |              (��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  �  �  |              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              X��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              �
�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              �j�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              L'�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              T7�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              l+�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              I�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              �]�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              �M�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'              Щ�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              |��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                  �  �  �)              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                  �     @+              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                      �,              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                      �-              �/�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                      �/              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                      �0              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                      �1              �~�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                  !  #  �2              t��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     k       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 r       CHARACTER,  canNavigate �3      �3      (4    |       LOGICAL,    closeQuery  4      44      `4   
 �       LOGICAL,    columnProps @4      l4      �4    �       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    �       CHARACTER,  hasForeignKeyChanged    88      d8      �8          LOGICAL,    openDataQuery   |8      �8      �8    )      LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	 7      LOGICAL,    prepareQuery    9      49      d9    A      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    N      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 [      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 e      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 o      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    y      CHARACTER,  assignDBRow                             <  �;      ��                  	    <              ���                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                      L=              8I�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                      �>              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                      �?              �m�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                       A              �q�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                       PB              |g�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                  "  #  PC              �i�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                  %  &  PD              �j�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                  (  )  PE              f�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                  +  ,  PF               -�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  .  0  \G              �-�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  2  3  �H              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  5  7  �I              �1�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  9  :  �J              `Z�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  <  =  �K              9�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  ?  B  �L              �9�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP          CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P          CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q     "      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !  /      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  @      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  O      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  ^      CHARACTER,  getForeignValues    @R      lR      �R  %  m      CHARACTER,  getQueryPosition    �R      �R      �R  &  ~      CHARACTER,  getQuerySort    �R      �R      S  '  �      CHARACTER,  getQueryString  �R      (S      XS  (  �      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /        CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0        CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1  +      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2  ;      LOGICAL,    removeQuerySelection    �W      �W      (X  3  L      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  a      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 o      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  z      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  �      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              @Ҏ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]              �Ҏ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              ȕ�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              @��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              0C�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b               D�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              <�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �  �  Hd              �#�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  �      CHARACTER,  getASBound  �d      e      0e  = 
 �      LOGICAL,    getAsDivision   e      <e      le  >  �      CHARACTER,  getASHandle Le      xe      �e  ?  �      HANDLE, getASHasStarted �e      �e      �e  @  	      LOGICAL,    getASInfo   �e      �e      f  A 	       CHARACTER,  getASInitializeOnRun    �e       f      Xf  B  #      LOGICAL,    getASUsePrompt  8f      df      �f  C  8      LOGICAL,    getServerFileName   tf      �f      �f  D  G      CHARACTER,  getServerOperatingMode  �f      �f      g  E  Y      CHARACTER,  runServerProcedure  �f      $g      Xg  F  p      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              R.                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              �.                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              ��.                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              $�.                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              �.                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              <�.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              ��.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              $�.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              ��.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              @�.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              ��.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              8�.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              LX4                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z              8p4                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �  �  �{              <G4                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  �  �  �|              J4                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                  �    x~              hQ4                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                      �              �5                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                      ��               �4                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                      L�              �;5                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                      t�              T�4                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 P      LOGICAL,    assignLinkProperty  ؃      �      8�  P  [      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  n      CHARACTER,  getChildDataKey ��      ̄      ��  R  |      CHARACTER,  getContainerHandle  ܄      �      <�  S  �      HANDLE, getContainerHidden  �      D�      x�  T  �      LOGICAL,    getContainerSource  X�      ��      ��  U  �      HANDLE, getContainerSourceEvents    ��      ��      ��  V  �      CHARACTER,  getContainerType    ܅      �      <�  W  �      CHARACTER,  getDataLinksEnabled �      H�      |�  X  �      LOGICAL,    getDataSource   \�      ��      ��  Y        HANDLE, getDataSourceEvents ��      ��      �  Z        CHARACTER,  getDataSourceNames  Ԇ       �      4�  [  %      CHARACTER,  getDataTarget   �      @�      p�  \  8      CHARACTER,  getDataTargetEvents P�      |�      ��  ]  F      CHARACTER,  getDBAware  ��      ��      �  ^ 
 Z      LOGICAL,    getDesignDataObject ȇ      �      (�  _  e      CHARACTER,  getDynamicObject    �      4�      h�  `  y      LOGICAL,    getInstanceProperties   H�      t�      ��  a  �      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  �      CHARACTER,  getObjectHidden �      <�      l�  d  �      LOGICAL,    getObjectInitialized    L�      x�      ��  e  �      LOGICAL,    getObjectName   ��      ��      �  f  �      CHARACTER,  getObjectPage   ̉      ��      (�  g  �      INTEGER,    getObjectParent �      4�      d�  h        HANDLE, getObjectVersion    D�      l�      ��  i        CHARACTER,  getObjectVersionNumber  ��      ��      �  j  )      CHARACTER,  getParentDataKey    Ċ      ��      $�  k  @      CHARACTER,  getPassThroughLinks �      0�      d�  l  Q      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  e      CHARACTER,  getPhysicalVersion  ��      ��      �  n  {      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  �      CHARACTER,  getQueryObject  �      4�      d�  p  �      LOGICAL,    getRunAttribute D�      p�      ��  q  �      CHARACTER,  getSupportedLinks   ��      ��      ��  r  �      CHARACTER,  getTranslatableProperties   ��      �      (�  s  �      CHARACTER,  getUIBMode  �      4�      `�  t 
 �      CHARACTER,  getUserProperty @�      l�      ��  u  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  	      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  '	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  4	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  @	      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  N	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  [	      CHARACTER,  setChildDataKey 4�      `�      ��  }  j	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  z	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    �	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  
      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  
      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 $
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  /
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  C
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  T
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  j
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  �
      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  �
      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �        LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �        LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �  0      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 J      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  U      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  e      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 q      CHARACTER,INPUT pcName CHARACTER    l�    (  ��  0�      �       4   �����                 @�                      ��                  )  V                  \&                       )  Ě        *  \�  ؛      �       4   �����                 �                      ��                  +  U                  �&                       +  l�  �    B  �  ��      �       4   �����                 ��                      ��                  N  P                  d�                       N  �         O                                  ,     
                    � ߱        �  $  R  ��  ���                           $  T  @�  ���                       x                         � ߱        x�    Z  ��  �      �      4   �����                �                      ��                  [  	                  ȷ                       [  ��  H�  o   ^      ,                                 ��  $   _  t�  ���                       �  @         �              � ߱        ��  �   `        Ȟ  �   a  �      ܞ  �   c        �  �   e  x      �  �   g  �      �  �   i  `      ,�  �   j  �      @�  �   k        T�  �   n  �      h�  �   p         |�  �   q  |      ��  �   s  �      ��  �   t  t      ��  �   u  �      ̟  �   v  ,      ��  �   w  �      ��  �   }  �      �  �     P	      �  �   �  �	      0�  �   �   
      D�  �   �  t
      X�  �   �  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  F	  t	  ��              |�                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ Z	  آ  ���                           O   r	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  y                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  
                  H�                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    K
  T�  Ц      x      4   ����x                �                      ��                  L
  �
                  �T                       L
  d�  ��  �   N
  �      �  �   O
  T      �  �   P
  �      0�  �   Q
  D      D�  �   R
  �      X�  �   S
  �      l�  �   U
  p      ��  �   V
  �      ��  �   W
  X      ��  �   X
  �      ��  �   Y
  �      Ч  �   Z
  D       �  �   [
  �       ��  �   \
  �       �  �   ]
  x!       �  �   ^
  �!      4�  �   _
  h"      H�  �   `
  �"      \�  �   a
  `#      p�  �   b
  �#      ��  �   c
  X$      ��  �   d
  �$      ��  �   e
  �$      ��  �   f
  L%      Ԩ  �   g
  �%      �  �   h
  <&      ��  �   i
  �&      �  �   j
  4'      $�  �   k
  �'      8�  �   l
  ,(      L�  �   m
  h(      `�  �   o
  �(      t�  �   p
  X)      ��  �   q
  �)      ��  �   r
  *      ��  �   s
  �*      ĩ  �   t
  �*      ة  �   u
  l+      �  �   v
  �+       �  �   w
  \,      �  �   x
  �,      (�  �   y
  L-      <�  �   z
  �-      P�  �   {
  <.      d�  �   |
  �.      x�  �   }
  4/      ��  �   ~
  �/          �   
  $0      d�    �
  ��  8�      T0      4   ����T0                H�                      ��                  �
  �                  ha                       �
  ̪  \�  �   �
  �0      p�  �   �
  (1      ��  �   �
  �1      ��  �      2      ��  �     �2      ��  �     3      ԫ  �     |3      �  �     �3      ��  �     t4      �  �     �4      $�  �     l5      8�  �     �5      L�  �   	  d6      `�  �   
  �6      t�  �     L7      ��  �     �7      ��  �     <8      ��  �     �8      Ĭ  �     ,9      ج  �     �9      �  �     :       �  �     X:      �  �     �:      (�  �     H;      <�  �     �;      P�  �     8<          �     �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  7                  �c                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
     �  Y                  ���                       �  <�  ̱  �   �  �K      $�  $  �  ��  ���                       �K     
                    � ߱        8�  �   �  L      ��  $   �  d�  ���                       ,L  @         L              � ߱        L�  $    ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V     �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $  (  x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   :  �  ���                                      ̵                      ��                  [  �                  ���                       [  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   p  �  ���                        adm-clone-props �  ��              �     W     `                          \  s                     start-super-proc    �  d�  �           �     X                                  �                     l�      �   �      �X      4   �����X      /     ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $  +  ��  ���                       @Y                         � ߱        ��    ;  �  \�  ��  \Y      4   ����\Y                и                      ��                  <  @                  \��                       <  �  pY                     �Y                     �Y                         � ߱            $  =  l�  ���                             A  �  T�      �Y      4   �����Y  �Y                         � ߱            $  B  (�  ���                       �Y                         � ߱        ع  $  F  ��  ���                       Ժ    I  ��  �  \�  �Y      4   �����Y      $  J  0�  ���                       Z                         � ߱            �   g  ,Z      lZ     
                �Z                     8\  @        
 �[              � ߱         �  V   {  p�  ���                        �  �   �  D\      �    -  0�  @�      �\      4   �����\      /   .  l�     |�                          3   �����\            ��                      3   �����\  �\     
                P]                     �^  @        
 `^              � ߱        ��  V   :  ��  ���                        �^     
                h_                     �`  @        
 x`              � ߱        ̼  V   ^  <�  ���                        L�    �  �  d�      �`      4   �����`                t�                      ��                  �  �                  t�                       �  ��  �  /   �  ��     ��                          3   �����`            н                      3   �����`      /   �  �     �                          3   ����a            <�                      3   ����8a  ��  /  K  x�         la                      3   ����Ta  initProps   x�  ��              H     Y     @                          <  A  	                                   ̿          t�  \�      ��                      ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��      K                      ��          ��  p     $t  �        t�  d�     0t                                        ��                    (                  ���                         ��   �  ��     Dt                                        ��                  )  E                  (��                       )  ��  ��  |�     Xt                                        ��                  F  b                  ���                       F  �  �  �     lt                                        ��                  c                    ���                       c  ��  ��  ��     �t                                        ��                  �  �                  ���                       �  (�  0�   �     �t                                        ��                  �  �                  d��                       �  ��  ��  ��     �t                                        ��                  �  �                  ,��                       �  @�  H�  8�     �t                                        ��                  �  �                  ���                       �  ��  ��  ��     �t  	                                      ��             	     �                    ���                       �  X�  `�  P�     �t  
                                      ��             
       -                  ���                         ��  ��  ��     �t                                        ��                  .  J                  �y�                       .  p�  x�  h�     u                                        ��                  K  g                  �z�                       K  ��  �  ��      u                                        ��                  h  �                  �{�                       h  ��  ��  ��     4u                                        ��                  �  �                  X|�                       �  �  �  �     Hu                                        ��                  �  �                  4=�                       �  ��  ��  ��     \u                                        ��                  �  �                  �=�                       �  ,�  4�  $�     pu                                        ��                  �  �                  �>�                       �  ��      ��     �u                                        ��                  �                    P?�                       �  D�      O     ��  ��  �u               D�          ,�  8�   , �                                                       �     ��                            ����                            ��  d�  ��  �      ��     Z     L�                      � H�  ]                     ��    .  �  ��      �u      4   �����u                ��                      ��                  /  C                  �@�                       /  �  ��  /   0  ��     ��                          3   �����u            ��                      3   �����u  h�  /   1  (�     8�                          3   �����u            X�                      3   ����v  ��  /   6  ��     ��                          3   ����(v            ��                      3   ����Hv      /   <   �     �                          3   ����hv            0�                      3   �����v  �v     
                $w                     tx  @        
 4x              � ߱        ��  V   �  @�  ���                        ��  $  �  ��  ���                       �x                         � ߱        �x     
                $y                     tz  @        
 4z              � ߱        ��  V   �  (�  ���                        t�  $  �  ��  ���                       �z     
                    � ߱        �z     
                {                     `|  @        
  |              � ߱        ��  V   �  �  ���                        \�  $  �  ��  ���                       l|     
                    � ߱        �|     
                �|                     L~  @        
 ~              � ߱        ��  V     ��  ���                        D�  $    ��  ���                       d~                         � ߱        �~     
                                     X�  @        
 �              � ߱        p�  V   '  ��  ���                        ��  �   A  p�      @�  $  B  ��  ���                       ��     
                    � ߱        ��     
                 �                     p�  @        
 0�              � ߱        l�  V   L  ��  ���                        ��  $  f  ��  ���                       |�     
                    � ߱        ��  �   �  ��      0�  $  �  �  ���                       Ђ     
                    � ߱        D�  �   �  �      ��  $  �  p�  ���                       $�                         � ߱              �  ��  ��      @�      4   ����@�      /   �  ��     �                          3   ����`�  4�     
   $�                      3   ������  d�        T�                      3   ������  ��        ��                      3   ������            ��                      3   ������  pushRowObjUpdTable  ��  ��  �                   [      �                               �                     pushTableAndValidate    ��  4�  �           |     \     �                          �  �                     remoteCommit    L�  ��  �           p     ]     �                          �                       serverCommit    ��  �  �           l     ^     �                          �  ,                                     4�          �  ��      ��                  �    �              �2�                    O   ����    e�          O   ����    R�          O   ����    ��          O      ��  ��  �    ��                            ����                            $�  P�      ��              _      L�                      
�     9                     disable_UI  ��  ��                      `      �                               L  
                    �  �    ����  �       ��       ��  8   ����   ��  8   ����       8   ����       8   ����       ��  ��      viewObject  ,   ��  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  �  4�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  �  x�  ��      returnFocus ,INPUT hTarget HANDLE   h�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  ��  �      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  \�  l�      removeAllLinks  ,   L�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE p�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  t�  ��      hideObject  ,   d�  ��  ��      exitObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  ��  ��      displayLinks    ,   ��  �  �      createControls  ,   ��  (�  8�      changeCursor    ,INPUT pcCursor CHARACTER   �  d�  p�      applyEntry  ,INPUT pcField CHARACTER    T�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  �  �      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  h�  p�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE X�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  �      runServerObject ,INPUT phAppService HANDLE  ��  8�  L�      disconnectObject    ,   (�  `�  p�      destroyObject   ,   P�  ��  ��      bindServer  ,   t�  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  ��  �      startFilter ,   ��  �  (�      releaseDBRow    ,   �  <�  L�      refetchDBRow    ,INPUT phRowObjUpd HANDLE   ,�  x�  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE h�  ��  ��      fetchDBRowForUpdate ,   ��  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  (�  8�      compareDBRow    ,   �  L�  `�      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   <�  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  �  �      updateState ,INPUT pcState CHARACTER    ��  D�  X�      updateQueryPosition ,   4�  l�  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    \�  ��  ��      undoTransaction ,   ��  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  x�  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   h�  ��  �      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  P�  d�      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  @�  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  ,�  @�      startServerObject   ,   �  T�  d�      setPropertyList ,INPUT pcProperties CHARACTER   D�  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ��   �      rowObjectState  ,INPUT pcState CHARACTER    ��  ,�  <�      retrieveFilter  ,   �  P�  d�      restartServerObject ,   @�  x�  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   h�  ��  ��      refreshRow  ,   p�  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  �  $�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  �  T�  l�      initializeServerObject  ,   D�  ��  ��      initializeObject    ,   p�  ��  ��      home    ,   ��  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��   �  �      fetchPrev   ,   ��   �  ,�      fetchNext   ,   �  @�  L�      fetchLast   ,   0�  `�  l�      fetchFirst  ,   P�  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   p�  ��  ��      endClientDataRequest    ,   ��  ��  ��      destroyServerObject ,   ��  �  �      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    ��  h�  x�      dataAvailable   ,INPUT pcRelative CHARACTER X�  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  ��  �      commitTransaction   ,   ��   �  0�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    �  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        �� "   H   %               � 
" 
   
 �%              h �P  \         (          
�                          
�            � {   3
" 
   
 
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 ��               1� �  
 �� �   �%               o%   o           � �    �
"   
 ��           �    1� �   �� �   �%               o%   o           � �   �
"   
 ��           �    1� �  
 �� �   �%               o%   o           � �   �
"   
 ��           l    1� �   �� �   �%               o%   o           � �    �
"   
 ��           �    1� �   �� �   �%               o%   o           � �   �
"   
 ��           T    1� �   �� 
   �%               o%   o           %               
"   
 ��          �    1�    �� "     
"   
 ��               1� )   �� �   �%               o%   o           � <  �
"   
 ��           �    1� >   �� �   �%               o%   o           � M  S �
"   
 ��           �    1� �   �� 
   �%               o%   o           %               
"   
 ��           p    1� �   �� 
   �%               o%   o           %               
"   
 ��           �    1� �   �� 
   �%               o%   o           %              
"   
 ��          h    1� �   �� 
     
"   
 ��           �    1� �  
 �� 
   �%               o%   o           %               
"   
 ��                1� �   �� �   �%               o%   o           � �    �
"   
 ��          �    1� �   �� "     
"   
 ��           �    1�    �� �   �%               o%   o           �   t �
"   
 ��          D	    1� �  
 �� "     
"   
 ��           �	    1� �   �� �   �%               o%   o           � �  � �
"   
 ��           �	    1� 6   �� �   �%               o%   o           � �    �
"   
 ��           h
    1� M  
 �� X   �%               o%   o           %               
"   
 �           �
    1� \   � 
   �%               o%   o           %              
"   
 �           `    1� d   � �   �%               o%   o           � �    
"   
 �           �    1� u   � �   �%               o%   o           o%   o           
"   
 �           P    1� �  
 � �   �%               o%   o           � �    
"   
 �           �    1� �   � �  	 �%               o%   o           � �  / 
"   
 ��          8    1� �   �� �  	   
"   
 �           t    1� �   � �  	 �o%   o           o%   o           � �    
"   
 ��          �    1�     �� �  	   
"   
 �           $    1�    � �  	 �o%   o           o%   o           � �    
"   
 ��          �    1�    �� 
     
"   
 ��          �    1� -   �� �  	   
"   
 ��              1� :   �� �  	   
"   
 ��          L    1� G   �� �  	   
"   
 �           �    1� U   � 
   �o%   o           o%   o           %              
"   
 ��              1� f   �� �  	   
"   
 ��          @    1� t  
 ��      
"   
 ��          |    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          0    1� �   �� �  	   
"   
 ��          l    1� �  	 �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 �                1�    � �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
 3(�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�            ��      p�               �L
�    %              � 8          � $         �           
�    � 7     
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� :  
 � �   �%               o%   o           � �    
"   
 �           <    1� E  
 � �   �%               o%   o           o%   o           
"   
 �           �    1� P   � "   �%               o%   o           o%   o           
"   
 �           4    1� Y   � 
   �%               o%   o           %               
"   
 �           �    1� h   � 
   �%               o%   o           %               
"   
 ��           ,    1� u   �� �   �%               o%   o           � �    
"   
 �           �    1� |   � 
   �%               o%   o           %              
"   
 �               1� �   � 
   �%               o%   o           o%   o           
"   
 �           �    1� �   � �   �%               o%   o           o%   o           
"   
 �               1� �  	 � �   �%               o%   o           � �    
"   
 �           �    1� �   � �   �%               o%   o           o%   o           
"   
 �               1� �   � �   �%               o%   o           o%   o           
"   
 �           �    1� �   � 
   �%               o%   o           %               
"   
 �           �    1� �   � 
   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� �  
 � 
   �%               o%   o           %              
"   
 �           H    1� �   � �   �%               o%   o           o%   o           
"   
 �           �    1�    � �   �%               o%   o           � �    
"   
 �           8    1�    � �   �%               o%   o           o%   o           
"   
 ��          �    1� "   �� "     
"   
 �           �    1� /   � �   �%               o%   o           � B  ! 
"   
 �           d    1� d   � �   �%               o%   o           � �    
"   
 �           �    1� q   � �   �%               o%   o           � �   
"   
 ��          L    1� �   �� �     
"   
 ��          �    1� �   �� "     
"   
 �           �    1� �   � �   �%               o%   o           � �    
"   
 ��          8     1� �  
 �� "     
"   
 ��           t     1� �   �� 
   �%               o%   o           o%   o           
"   
 �           �     1� �   � 
   �%               o%   o           %               
"   
 �           l!    1� �   � 
   �%               o%   o           %               
"   
 �           �!    1� �   � �   �%               o%   o           � �    
"   
 �           \"    1�    � �   �%               o%   o           o%   o           
"   
 �           �"    1�    � 
   �%               o%   o           %              
"   
 �           T#    1� *   � 
   �%               o%   o           %               
"   
 ��           �#    1� 7   �� 
   �%               o%   o           %               
"   
 ��          L$    1� G   �� "     
"   
 ��          �$    1� T   �� �     
"   
 �           �$    1� a   � X   �%               o%   o           o%   o           
"   
 �           @%    1� m   � �   �%               o%   o           � �    
"   
 �           �%    1� {   � �   �%               o%   o           o%   o           
"   
 �           0&    1� �   � 
   �o%   o           o%   o           o%   o           
"   
 �           �&    1� �   � �  	 �%               o%   o           o%   o           
"   
 �           ('    1� �   � �   �%               o%   o           o%   o           
"   
 �           �'    1� �  
 � X   �%               o%   o           o%   o           
"   
 ��           (    1� �   �� �     
"   
 �           \(    1� �   � �   �%               o%   o           � �  4 
"   
 �           �(    1� $  
 � 
   �%               o%   o           %              
"   
 ��          L)    1� /   �� "     
"   
 �           �)    1� @   � �   �%               o%   o           � �    �
"   
 �           �)    1� N   � 
   �%               o%   o           %              
"   
 �           x*    1� ]   � �   �%               o%   o           � �    
"   
 �           �*    1� j   � �   �%               o%   o           � �    
"   
 �           `+    1� x   � �   �%               o%   o           � �    
"   
 �           �+    1� �   � 
   �%               o%   o           %               
"   
 �           P,    1� �  	 � "   �%               o%   o           o%   o           
"   
 ��           �,    1� �   �� �   �%               o%   o           � �  	 
"   
 �           @-    1� �   � X   �%               o%   o           %       �       
"   
 �           �-    1� �   � �   �%               o%   o           � �    
"   
 �           0.    1� �   � 
   �o%   o           o%   o           %              
"   
 �           �.    1� �   � 
   �%               o%   o           %               
"   
 �           (/    1� �   � �   �%               o%   o           o%   o           
"   
 �           �/    1�    � �  	 �%               o%   o           � �    
"   
 ��          0    1�    �� �  	   P �L 
�H T   %              �     }        �GG %              
"   
 ��           �0    1� !  
 �� �   �%               o%   o           � �    �
"   
 �           1    1� ,   � 
   �%               o%   o           %               
"   
 �           �1    1� 9  	 � �   �%               o%   o           � �    
"   
 �           2    1� C   � �   �%               o%   o           � �    
"   
 �           �2    1� Q   � 
   �%               o%   o           %               
"   
 �           �2    1� a   � �   �%               o%   o           � �    
"   
 �           p3    1� t   � �   �%               o%   o           o%   o           
"   
 �           �3    1� |   � �   �%               o%   o           o%   o           
"   
 �           h4    1� �   � 
   �%               o%   o           o%   o           
"   
 ��           �4    1� �   �� 
   �%               o%   o           o%   o           
"   
 �           `5    1� �   � 
   �%               o%   o           o%   o           
"   
 �           �5    1� �   � �   �%               o%   o           o%   o           
"   
 �           X6    1� �  	 � �  	 �%               o%   o           � �    
"   
 �           �6    1� �  
 � �  	 �%               o%   o           � �    
"   
 �           @7    1� �   � �   �%               o%   o           � �    
"   
 �           �7    1� �   � �   �%               o%   o           o%   o           
"   
 �           08    1� �   � �   �%               o%   o           o%   o           
"   
 �           �8    1�    � �   �%               o%   o           � �    
"   
 �            9    1�    � �   �%               o%   o           � �    
"   
 �           �9    1� *   � �  	 �%               o%   o           o%   o           
"   
 ��          :    1� <   �� "     
"   
 �           L:    1� H   � �   �%               o%   o           � �    
"   
 �           �:    1� V   � �   �%               o%   o           o%   o           
"   
 ��           <;    1� i   �� 
   �%               o%   o           o%   o           
"   
 �           �;    1� {  
 � �   �%               o%   o           � �    
"   
 �           ,<    1� �   � �   �%               o%   o           � �    
"   
 �           �<    1� �   � 
   �%               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 �           p=    1� �  	 � "   �%               o%   o           o%   o           
"   
 �           �=    1� �   � "   �%               o%   o           o%   o           
"   
 �           h>    1� �   � "   �%               o%   o           o%   o           
"   
 ��           �>    1� �   �� 
   �%               o%   o           %              
"   
 �           `?    1� �   � �   �%               o%   o           �   M �
"   
 �           �?    1� R   � 
   �%               o%   o           %              
"   
 �           P@    1� c   � 
   �%               o%   o           %               
"   
 �           �@    1� w   � 
   �%               o%   o           %               
"   
 �           HA    1� �   � �  	 �%               o%   o           � �   
"   
 �           �A    1� �   � 
   �%               o%   o           %               
"   
 �           8B    1� �   � �  	 �%               o%   o           o%   o           
"   
 �           �B    1� �   � 
   �o%   o           o%   o           %              
"   
 ��           0C    1� �   �� �  	 �o%   o           o%   o           � �    �
"   
 �           �C    1� �   � "   �o%   o           o%   o           o%   o           
"   
 �            D    1� �   � "   �o%   o           o%   o           o%   o           
"   
 �           �D    1�    � �  	 �o%   o           o%   o           o%   o           
"   
 �           E    1�    � "   �o%   o           o%   o           o%   o           
"   
 �           �E    1� -   � �  	 �o%   o           o%   o           � ;   
"   
 �           F    1� =   � �  	 �o%   o           o%   o           � L   
"   
 �           |F    1� X   � 
   �%               o%   o           %               
"   
 �           �F    1� l   � 
   �%               o%   o           %               
"   
 ��          tG    1� �   �� �  	   
"   
 �           �G    1� �   � 
   �%               o%   o           %               
"   
 �           ,H    1� �   � �   �%               o%   o           o%   o           
"   
 �           �H    1� �   � �   �%               o%   o           o%   o           
"   
 �           $I    1� �   � 
   �%               o%   o           o%   o           
"   
 �           �I    1� �   � �   �%               o%   o           � �    
"   
 �           J    1� �   � �   �%               o%   o           %               
"   
 �           �J    1� �  	 � 
   �%               o%   o           %                "    �%     start-super-proc b�%     adm2/smart.p �3P �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6�      
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 3
"   
 �
"   
 3
"   
   (�  L ( l       �        �M    ��    � P   �        �M    �@    
� @  , 
�       �M    ��    3p�               �L
�    %              � 8       N    � $         �           
�    � 7   3
"   
 �p� @  , 
�       O    �� )   �p�               �L"  	  , �   � 5   � 7   ��     }        �A      |    "  	    � 5   %              (<   \ (    |    �     }        �A� 9   �A"  
      "  	  3"  
    < "  	  3"  
  (    |    �     }        �A� 9   �A"  
  
�H T   %              �     }        �GG %              
"   
 3
"   
 �
"   
 3
"   
   (�  L ( l       �        �P    ��    � P   �        �P    �@    
� @  , 
�       �P    ��    3p�               �L
�    %              � 8      Q    � $         �           
�    � 7   3
"   
 �p� @  , 
�       R    �� �  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        �R    ��    � P   �        �R    �@    
� @  , 
�       �R    ��      p�               �L
�    %              � 8      �R    � $         �           
�    � 7     
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� �     p�               �L%               
"   
  p� @  , 
�       �T    ��     p�               �L%               
"   
  p� @  , 
�       U    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
  (   � 
"   
 3    �        �U    ��    �
"   
   � 8      DV    � $         �           
�    � 7   3
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6�      
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � b   
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 3    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 "      �       }        �
"   
 �%              %                "    �%     start-super-proc a�%     adm2/appserver.p ��    � �     
�    �     }        �%               %      Server  - �     }        �    "    � �    �%               %      Client      "    � �    �%      NONE    p�,  8         $     "            �    3
�    
�H T   %              �     }        �GG %              
"   
 3
"   
 �
"   
 3
"   
   (�  L ( l       �        �Z    ��    � P   �        �Z    �@    
� @  , 
�       �Z    ��    3p�               �L
�    %              � 8      �Z    � $         �           
�    � 7   3
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "            �    3
�     "    �%     start-super-proc a�%     adm2/dataquery.p �
�H T   %              �     }        �GG %              
"   
 3
"   
 �
"   
 3
"   
 3(�  L ( l       �         ]    ��    � P   �        ,]    �@    
� @  , 
�       8]    ��    3p�               �L
�    %              � 8      D]    � $         �    3     
�    � 7   3
"   
 �p� @  , 
�       T^    �� �   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 3
"   
 �
"   
 3
"   
 3(�  L ( l       �        8_    ��    � P   �        D_    �@    
� @  , 
�       P_    ��    3p�               �L
�    %              � 8      \_    � $         �    3     
�    � 7   3
"   
 �p� @  , 
�       l`    �� �   �p�               �L%               "    �%     start-super-proc `�%     adm2/query.p �3%     start-super-proc `�%     adm2/queryext.p % 	    initProps 3
�    %` V P   FOR EACH Almmmatg NO-LOCK,       FIRST Almmmate OF Almmmatg NO-LOCK INDEXED-REPOSITION �   � �     � �     � �         "    � �    �
�H T   %              �     }        �GG %              
"   
 3
"   
 �
"   
 3
"   
   (�  L ( l       �        pb    ��    � P   �        |b    �@    
� @  , 
�       �b    ��    3p�               �L
�    %              � 8      �b    � $         �           
�    � 7   3
"   
 �p� @  , 
�       �c    �� 9  	 �p�               �L"    , %               �    "      � �         %              %                   "      %                  "      "      "     T(        "    �%              "    �� �   �"      �       "    3�    "    �� 9   �� �      � 9   3�    "     � 9    S    "      "    �    "    %                � @    �     t T     P   4       �"      (0       4       "      � �      � �    3� �   T ,  %              T   "    "    �� �     � 9   3� �   T    �    "    � 9   �"      � 9   3"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    3%              � �    �� �     4  �     "      
�H T   %              �     }        �GG %              
"   
 3
"   
 �
"   
 3
"   
   (�  L ( l       �        lh    ��    � P   �        xh    �@    
� @  , 
�       �h    ��    3p�               �L
�    %              � 8      �h    � $         �           
�    � 7   3
"   
 �p� @  , 
�       �i    �� !  
 �p�               �L"    ,       "  
  ��    � �    � �   �      "  	    �    � �  ) �� �   �   � �     � �     � �    3�   � �     � �   3� �  )       "  
  �    � �    �� �   �      "  	    �    � �   �� �   �   ,        "    3� �   ��   � �   3� �   �� �    �   ,        "      � �     �   � �   �� �   �� �   �   � �     � �     �   >   
�H T   %              �     }        �GG %              
"   
 �
"   
 3
"   
 �
"   
 �(�  L ( l       �        @l    ��    � P   �        Ll    �@    
� @  , 
�       Xl    ��    �p�               �L
�    %              � 8      dl    � $         �           
�    � 7     
"   
 �p� @  , 
�       tm    �� �   �p�               �L"    , 
"   
   p� @  , 
�       �m    �� �     p�               �L"    , 
"   
  p� @  , 
�       $n    �� a    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � �      � �         "  	  ��     "    T    "      "      @ A,    �   � �   �� �     "    3"       T      @   "    �(        "      � �    3� �      � �   3"    �     "  	   %              D H   @ A,    �   � �   3� �     "    3"    �,    S   "    3� �    �� �   �%                T      @   "    �(        "      � �    3� �      � �   3"         "  
   %                         "    �� �     "    3           "      � �   3"      
�H T   %              �     }        �GG %              
"   
 
"   
   
"   
 
"   
 3(�  L ( l       �        @r    ��    � P   �        Lr    �@    
� @  , 
�       Xr    ��    p�               �L
�    %              � 8      dr    � $         �    3     
�    � 7   �
"   
 �p� @  , 
�       ts    �� �   �p�               �L"    , 
"   
   p� @  , 
�       �s    �� a     p�               �L"    , "      %              %              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    �%     start-super-proc _�%     adm2/data.p %     start-super-proc _�%     adm2/dataext.p %     start-super-proc _�%     adm2/dataextcols.p %     start-super-proc _�%     adm2/dataextapi.p �
�H T   %              �     }        �GG %              
"   
 3
"   
 �
"   
 3
"   
 3(�  L ( l       �        �v    ��    � P   �         w    �@    
� @  , 
�       w    ��    3p�               �L
�    %              � 8      w    � $         �    3     
�    � 7   3
"   
 �p� @  , 
�       (x    �� �   �p�               �L%               %     "alm/dmate-matg.i"  
�H T   %              �     }        �GG %              
"   
 3
"   
 �
"   
 3
"   
   (�  L ( l       �        �x    ��    � P   �         y    �@    
� @  , 
�       y    ��    3p�               �L
�    %              � 8      y    � $         �           
�    � 7   3
"   
 �p� @  , 
�       (z    �� �   �p�               �L"    , 
�     	         �G
�H T   %              �     }        �GG %              
"   
 3
"   
 �
"   
 3
"   
   (�  L ( l       �        �z    ��    � P   �        �z    �@    
� @  , 
�       �z    ��    3p�               �L
�    %              � 8      {    � $         �           
�    � 7   3
"   
 �p� @  , 
�       |    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 3
"   
 �
"   
 3
"   
   (�  L ( l       �        �|    ��    � P   �        �|    �@    
� @  , 
�       �|    ��    3p�               �L
�    %              � 8      �|    � $         �           
�    � 7   3
"   
 �p� @  , 
�        ~    �� �  	 �p�               �L
"   
 , 
"   
 �     � `  	   �        X~    �
�H T   %              �     }        �GG %              
"   
 3
"   
 �
"   
 3
"   
   (�  L ( l       �        �~    ��    � P   �        �~    �@    
� @  , 
�       �~    ��    3p�               �L
�    %              � 8      �~    � $         �           
�    � 7   3
"   
 �p� @  , 
�       �    �� �   �p�               �L"    , 
"   
   �       d�    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 3
"   
 �
"   
 3
"   
   (�  L ( l       �        ��    ��    � P   �        ��    �@    
� @  , 
�       �    ��    3p�               �L
�    %              � 8      �    � $         �           
�    � 7   3
"   
 �p� @  , 
�       $�    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
         � j   3
�    
�             �Gp�,  8         $     
"   
         � |   3
�    �    � �     
�        "    �� �    �%     modifyListProperty 
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    �      
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   l       ��                 h  �  �                ��                    O   ����    e�          O   ����    R�          O   ����    ��        $  w  �   ���                       �U     
                    � ߱              x  (  �      V      4   ����V                �                      ��                  y  �                  $��                       y  8  �  �  z  PV            |  �  `      �V      4   �����V                p                      ��                  }  �                  ���                       }  �  �  o   ~      ,                                 �  �     �V      �  �   �  �V      $  $  �  �  ���                        W     
                    � ߱        8  �   �  @W      L  �   �  `W      `  �   �  �W          $   �  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  4��                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��                 W  �  �               u�                    O   ����    e�          O   ����    R�          O   ����    ��        $  w  �   ���                       ta                         � ߱        d  $  x  8  ���                       �a                         � ߱             �  �  �      b      4   ����b  $b     
                �b                     �c  @        
 �c              � ߱            V   �  �  ���                        x  $  �  L  ���                       �c                         � ߱        <  $     �  ���                       d                         � ߱          L      �  �                      ��        0                             L��      �d     X       �      $    x  ���                       0d                         � ߱        �  $    �  ���                       `d                         � ߱            4   �����d  �d                     �d                     e                     Xe                     xe                         � ߱        �  $      ���                               �         �e      4   �����e      $    ,  ���                       �e          �f             � ߱          $    �  ���                       �f                         � ߱          ,      �  �                      ��        0           "                  ���      �g     �       �      $    X  ���                       g                         � ߱        �  $    �  ���                       <g                         � ߱            4   ����dg      $      ���                       �g                         � ߱         h     
                �h                     �i  @        
 �i              � ߱        T  V   -  D  ���                        �i       
       
       ,j       	       	       `j                     �j                         � ߱         	  $  t  �  ���                       �j       
       
       �j       	       	        k                     tk                         � ߱        ,	  $  �  �  ���                        
  $    X	  ���                       �k                         � ߱        �k     
                pl                     �m  @        
 �m          n  @        
 �m          pn  @        
 0n              � ߱        �
  V   $  �	  ���                          �
         �                      ��        0         �  �                  ���      �n     p     �  L
      $  �  �
  ���                       |n                         � ߱        x  $  �  L  ���                       �n                         � ߱        �  4   �����n      4   ����o  �  $  �  �  ���                       to                         � ߱            �    �      �o      4   �����o                �                      ��                  �  �                  � �                       �    �o                     @p       	       	           � ߱            $  �  �  ���                             �  (  �      hp      4   ����hp                �                      ��                  �  �                  x�                       �  8  �p                     dq       
       
           � ߱            $  �  �  ���                       �q                     �q                         � ߱          $  �  (  ���                       �q     
                pr                     �s  @        
 �s          t  @        
 �s              � ߱            V   �  �  ���                                    7           �  �  � |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             �   �   �   �   �   �   �   �       $  4  D  T  d  t  �  \  �  �  �  �  8HXhx��������(8HX      �   �   �   �   �   �   �   �      $  4  D  T  d  t  �   \ �  �  �  �  8HXhx��������(8HX  �                    � �                     �          ��                            ����                                                        �   l       ��                  d  o  �               ?�                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                  y  �  �               DR�                    O   ����    e�          O   ����    R�          O   ����    ��      �       �              �                  $                  d  /  �  $     4  ��                      3   ����ԃ            T                      3   ������      O   �  ��  ��  �               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  �  �  �               ,c�                    O   ����    e�          O   ����    R�          O   ����    ��      �       �              �                $                  �       ,             �          �                                �  /  �  t     �  ,�                      3   �����            �                      3   ����4�     /  �  �     �  \�                      3   ����@�  x                             3   ����d�      $   �  L  ���                                                   � ߱                  �  �                  3   ����p�      $   �  �  ���                                                   � ߱        X  $  �  ,  ���                       |�                         � ߱            O   �  ��  ��  ��               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                  �  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  �                    �          �                      �              /  �  L     \  Ȅ                      3   ������  �        |  �                  3   ����Є      $   �  �  ���                                                   � ߱                                      3   ����܄      $   �  @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                  J  U  �               �B�                    O   ����    e�          O   ����    R�          O   ����    ��            T  �   �       ��      4   ������      �   T  �    ��                            ����                            TXS appSrvUtils s-codcia s-codalm D:\newsie\on_in_co\aplic\alm\dmate-matg.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "alm/dmate-matg.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server Client NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH Almmmatg NO-LOCK,       FIRST Almmmate OF Almmmatg NO-LOCK INDEXED-REPOSITION ,   Almmmatg Almmmate  ; codfam DesMat DesMar UndStk FchIng Pesmat codmat StkAct CodUbi codfam codmat DesMat DesMar UndStk StkAct CodUbi FchIng Pesmat INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p codfam codmat DesMat DesMar UndStk StkAct CodUbi FchIng Pesmat RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery x  �.  �  4<      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   Z	  r	  t	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props w  x  y  z  |  }  ~    �  �  �  �  �  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable             |     cQueryString    |  �  "   Y   �          �                  initProps   w  x  �  �  �                     "  -  t  �    $  �  �  �  �  �  �  �  �  �  �  �  �  �  �            d     lRet              �        piTableIndex    �  �  (   Z   P  l      �                  deleteRecordStatic      (  )  E  F  b  c    �  �  �  �  �  �  �  �  �      -  .  J  K  g  h  �  �  �  �  �  �  �  �  �  �                         !       �  �     [       x      �                  pushRowObjUpdTable  o  �        �        pcValType                  $       �  \     \       �      D                  pushTableAndValidate    �  �  �  �        �        pcContext   �             $       �        �        pcMessages            �        pcUndoIds     ,     ]       h                        remoteCommit    �  �  �  �  �  X             $       |        p        pcMessages            �        pcUndoIds   �  �     ^       @      �                  serverCommit    �  �  �  ,     _                                 getRowObjUpdStatic       �  p     `               d                  disable_UI  T  U  4  �       D      �                      �  �  �     RowObject   l         t         |         �         �         �         �         �         �         �         �         �         �         �         codfam  codmat  DesMat  DesMar  UndStk  StkAct  CodUbi  FchIng  Pesmat  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �       RowObjUpd   �         �         �         �         �         �         �         �         �                                              (         4         codfam  codmat  DesMat  DesMar  UndStk  StkAct  CodUbi  FchIng  Pesmat  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   d          X  
   appSrvUtils �        x     s-codcia    �        �     s-codalm    �       �     xiRocketIndexLimit  �        �  
   gshAstraAppserver             
   gshSessionManager   @        0  
   gshRIManager    h        T  
   gshSecurityManager  �  	 	     |  
   gshProfileManager   �  
 
     �  
   gshRepositoryManager    �        �  
   gshTranslationManager           �  
   gshWebManager   0              gscSessionId    T        D     gsdSessionObj   x        h  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager   �        �     gsdTempUniqueID         �     gsdUserObj  ,             gsdRenderTypeObj    T        @     gsdSessionScopeObj  p       h  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf   �       �     glADMLoadFromRepos  �       �     glADMOk          
   ghContainer 8    	   ,     cObjectName T    
   L     iStart  t       h     cAppService �       �     cASDivision �       �     cServerOperatingMode    �       �     cContainerType         �     cQueryString    (         
   hRowObject  H       <  
   hDataQuery  h       \     cColumns             |     cDataFieldDefs  �    X  �  RowObject         X  �  RowObjUpd          "   >   �   �   �   �   (  )  *  +  B  N  O  P  R  T  U  V  Z  [  ^  _  `  a  c  e  g  i  j  k  n  p  q  s  t  u  v  w  }    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  K
  L
  N
  O
  P
  Q
  R
  S
  U
  V
  W
  X
  Y
  Z
  [
  \
  ]
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
                     	  
                            �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  7  �  �  �  �  �  �  �  �      (  :  Y  [  p  �      +  ;  <  =  @  A  B  F  I  J  g  {  �  -  .  :  ^  �  �  �  �  �  K  .  /  0  1  6  <  C  �  �  �  �  �  �      '  A  B  L  f  �  �  �  �  �  �      ��  D:\newsie\on_in_co\aplic\alm\dmate-matg.w    �#  ��  C:\Progress\OpenEdge\src\adm2\data.i ($  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    X$  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i �$  yr , D:\newsie\on_in_co\aplic\alm\dmate-matg.i    �$  �   C:\Progress\OpenEdge\src\adm2\query.i    %  z + C:\Progress\OpenEdge\src\adm2\delrecst.i 8%  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  l%   ) %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   �%  � ! C:\Progress\OpenEdge\src\adm2\dataquery.i    �%  �Z ( %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   &  �< " C:\Progress\OpenEdge\src\adm2\appserver.i    \&  �� ' %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �&  I� # C:\Progress\OpenEdge\src\adm2\smart.i    �&  Ds & C:\Progress\OpenEdge\gui\fn  '  tw % %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   4'  Q. $ C:\Progress\OpenEdge\gui\set t'  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i �'  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    �'  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    (  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  X(  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i �(  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i �(   
 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    )  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   H)  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   �)  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �)  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    *  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    P*  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �*  �j  C:\Progress\OpenEdge\gui\get �*  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �*  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    4+  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i x+  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �+  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �+  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i    ,  �  C:\Progress\OpenEdge\src\adm2\appsprto.i d,  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �,  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �,  ª  %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   -  �� 	 C:\Progress\OpenEdge\src\adm2\qryprto.i  d-  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   �-  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �-  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    .  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   T.  ��    D:\newsie\on_in_co\aplic\alm\dmate-matg_cl.w        {      �.  �   
     �.  [  �     �.     �  &   �.  �   0     /     �  .   /  �   �     $/     �     4/  �   �     D/     �  $   T/  �   �     d/     h  $   t/  �   f     �/     D  $   �/  �   A     �/       $   �/  �        �/     �  $   �/  �   �     �/     �  $   �/  �   �     0     �  $   0  �   �     $0     �  $   40  �   �     D0     i  -   T0  �   e     d0     [  ,   t0  k   !     �0  �        �0     �  +   �0  �  �      �0     �  +   �0  �  �      �0     �  +   �0  �  �      �0     �  +   1  �  �      1     �  +   $1  �  �      41     j  +   D1  �  g      T1     M  +   d1  �  J      t1     0  +   �1  �  -      �1       +   �1  �        �1     �  +   �1  �  �      �1     �  +   �1  �  �      �1     �  +   2  �  �      2     �  +   $2  �  �      42     �  +   D2  �        T2     e  +   d2  �  b      t2     H  +   �2  �  E      �2     +  +   �2  �  (      �2       +   �2  �  �      �2     �  $   �2  �  �      �2     �  $   3  j  �      3     b  $   $3  i  a      43     ?  $   D3  h  >      T3       $   d3  ^        t3     �  *   �3  ]  �      �3     �  *   �3  \  �      �3     �  *   �3  [  �      �3     w  *   �3  Z  v      �3     P  *   4  Y  O      4     )  *   $4  X  (      44       *   D4  W        T4     �  *   d4  V  �      t4     �  *   �4  U  �      �4     �  *   �4  T  �      �4     f  *   �4  S  e      �4     ?  *   �4  R  >      �4       *   5  Q        5     �  *   $5  P  �      45     �  *   D5  O  �      T5     �  *   d5  N  �      t5     |  *   �5  M  {      �5     U  *   �5  ?  G      �5     %  $   �5    �      �5     �  $   �5  �   G      �5     �  )   6  g   �      6  a   �  !   $6     z  (   46  _   x  !   D6     V  $   T6  ]   T  !   d6     2  $   t6  I     !   �6  �     "   �6     �  '   �6  �   �  "   �6     �  $   �6  �   �  "   �6     s  $   �6  �   q  "   �6     O  $   7  g   5  "   7          $7  O   �  "   47  �   �  #   D7     �  &   T7  �   V  #   d7     �  %   t7  �   �  #   �7     �  $   �7  �   �  #   �7     �  $   �7  �   �  #   �7     �  $   �7  �   �  #   �7     h  $   �7  �   T  #   8     2  $   8  }   &  #   $8       $   48     �  #   D8     :  "   T8     �  !   d8     �      t8     @     �8  �   7     �8  O   )     �8          �8     �     �8  �   �     �8  �   �     �8  O   z     �8     i     9          9  y   �
     $9  �   �
  
   49  G   �
     D9     �
     T9     �
     d9  c   #
  
   t9  x   
     �9  M   
     �9     �	     �9     �	     �9  a   �	     �9  �  q	     �9     R	     �9  �  	     �9  O   	     :      	     :     �     $:  �   �     4:     �     D:          T:  x   �     d:     �     t:     m     �:     i     �:     U     �:     <     �:  Q   ,     �:     �     �:     �     �:     �     �:     l     ;  ]   f  
   ;     \     $;       
   4;          D;     �  
   T;  Z   �     d;     �  	   t;     �     �;     �     �;     �     �;  c   p     �;     N     �;          �;     �      �;     �      �;     �      <     &      <           $<           