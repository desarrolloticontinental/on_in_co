	��V7�K�6   �                                              � 36F400EFutf-8 MAIN O:\on_in_co\Util\dalmcmov_cl.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,TipMov character 0 0,CodMov integer 1 0,NroSer integer 2 0,NroDoc integer 3 0,FchDoc date 4 0,RowNum integer 5 0,RowIdent character 6 0,RowMod character 7 0,RowIdentIdx character 8 0,RowUserProp character 9 0,ChangedFields character 10 0        �              �             -� �  �              ��              X;     +   �� �  W    � `  X   �� d  Y   �   [   ��   \   � <  ]   H�    ^   h� 0  `   ? �� �  iSO8859-1                                                                                �                                      �                   ��                `  �       �|   T�              ��  �   �      �                                                         PROGRESS                         �           
    
                    �              �                                                                                                     
  H  �      �  
    
                  �  x             4                                                                                          �          
  �  �      p  
    
                  \  $             �                                                                                          �          
  �          
    
                    �             �                                                                                                    
  L        �  
    
                  �  |             8                                                                                                    
  �  &      t  
    
                  `  (             �                                                                                          &          
  �  8         
    
                    �             �                                                                                          8          
  P  M      �  
    
                  �  �  	           <                                                                                          M          
  �  c      x  
    
                  d  ,  
           �                                                                                          c          
  �  q      $                           �             �                                                                                          q            T  ~      �                        �  �             @                                                                                          ~             	  �      |  
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
                                                                                          �            �  �      ,                          �             �                                                                                          �            \  �      �                        �  �             H                                                                                          �                �      �                        p               �                                                                                          �            p         �       s  X  �     �  s  �      ,  
       s             �          �      �              �       �  X  |     �  �  �!      �         �         �    T          D      �                 P�                                               T�          `  �  L l@                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                   �  �  �  �  �          �             �      0             4             L  T  X  x  h                         |  �  �  �  �          �             �  �  �  �  �          �                    (                             ,  8  @  L                              P  X  `  h                             l  x  �  �                             �  �  �  �                                                                          TipMov  X   Tipo de movimiento  Tp.!movmto.     Tipo de movimiento  CodMov  99  C�digo de movimiento    C�digo!movimto. 0   C�digo de movimiento    NroSer  999 Numero Serie    Numero!Serie    0   NroDoc  999999  No. documento   Numero de!documento 0   N�mero de documento FchDoc  99/99/9999  Fecha   Fecha!docum TODAY   Fecha de documento  RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ���������    �          "        2        9                �     i     i     i     		 	
 	    �          "  )  2  9  E                                                                                                                                     	                  
                                                                     T  \  `  �  t          �             �  �  �  �  �          �             �  �  �                                  (  L  8          P             d  l  x  �  �          �             �  �  �  �                             �  �  �  �                              �  �  �                                     (                             ,  8  @  L                              P  `  h  x                                                                          TipMov  X   Tipo de movimiento  Tp.!movmto.     Tipo de movimiento  CodMov  99  C�digo de movimiento    C�digo!movimto. 0   C�digo de movimiento    NroSer  999 Numero Serie    Numero!Serie    0   NroDoc  999999  No. documento   Numero de!documento 0   N�mero de documento FchDoc  99/99/9999  Fecha   Fecha!docum TODAY   Fecha de documento  RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  ���������    �          "        2        9                �     i     i     i     		 	
 	    �          "  )  2  9  E  Q    ��                            ����                            �    p�                    �    undefined                                                               �       t�  �   l   ��  ��                    �����               ��                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                     3   ����       $      H  ���                       8      
                       � ߱        �  �   "   D       �     >          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   �j�                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  S  V  L              6X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  X  ^  �              ��X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  `  a  p              |yY	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  c  f  p              0zY	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  h  j  �              ��X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  l  o  �	              \X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  q  r  H              �rX	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  t  u  T              �sX	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  w  y  T              8tX	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  {  |  |              8
X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  ~    |              �
X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  �  �  |              HX	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              h�X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              ��X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              X�X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              �VX	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              TWX	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              XX	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              p&X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              XY	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              �Y	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              `X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              ��V	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              X�W	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              �Q	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              PY	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              ��O	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              ��N	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              P3P	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'              �O	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              ��O	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                  �  �  �)              hv�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                  �  �  @+               9�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                  �  �  �,              �9�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                    
  �-              �m�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                      �/              Tc�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                      �0              �e�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                      �1              p�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                      �2              d#�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     K       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 R       CHARACTER,  canNavigate �3      �3      (4    \       LOGICAL,    closeQuery  4      44      `4   
 h       LOGICAL,    columnProps @4      l4      �4    s       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	        CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    �       CHARACTER,  hasForeignKeyChanged    88      d8      �8    �       LOGICAL,    openDataQuery   |8      �8      �8    	      LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	       LOGICAL,    prepareQuery    9      49      d9    !      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    .      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 ;      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 E      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 O      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    Y      CHARACTER,  assignDBRow                             <  �;      ��                  �    <              �'�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                      L=               �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                  
    �>              �'                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                      �?              `*                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                       A              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                      PB              \�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                      PC              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                      PD              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                      PE              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                  !  "  PF              `��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  $  &  \G              h��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  (  )  �H              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  +  -  �I              p��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  /  0  �J              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  2  3  �K              d��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  5  8  �L              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    z      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP    �      CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P    �      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q           CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !        CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "         HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  /      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  >      CHARACTER,  getForeignValues    @R      lR      �R  %  M      CHARACTER,  getQueryPosition    �R      �R      �R  &  ^      CHARACTER,  getQuerySort    �R      �R      S  '  o      CHARACTER,  getQueryString  �R      (S      XS  (  |      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /  �      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0  �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1        CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2        LOGICAL,    removeQuerySelection    �W      �W      (X  3  ,      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  A      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 O      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  Z      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  i      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  z      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              h�d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              �d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]              H�d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              �d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              P�d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a               �d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b              t�d	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �  �  Hd              h�d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  �      CHARACTER,  getASBound  �d      e      0e  = 
 �      LOGICAL,    getAsDivision   e      <e      le  >  �      CHARACTER,  getASHandle Le      xe      �e  ?  �      HANDLE, getASHasStarted �e      �e      �e  @  �      LOGICAL,    getASInfo   �e      �e      f  A 	 �      CHARACTER,  getASInitializeOnRun    �e       f      Xf  B        LOGICAL,    getASUsePrompt  8f      df      �f  C        LOGICAL,    getServerFileName   tf      �f      �f  D  '      CHARACTER,  getServerOperatingMode  �f      �f      g  E  9      CHARACTER,  runServerProcedure  �f      $g      Xg  F  P      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  c      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  q      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I        LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              He	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              T-e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              �4e	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              x<e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              $Ae	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              @Ee	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              ,He	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              4Ke	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              0Ne	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              �Ne	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              tOe	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              dRe	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              tSe	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z              `ce	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �  �  �{              �je	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  �  �  �|              Xke	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                  �  �  x~              �se	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                  �  �  �              �ze	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                  �    ��              d{e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                      L�              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                      t�              ȉe	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 0      LOGICAL,    assignLinkProperty  ؃      �      8�  P  ;      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  N      CHARACTER,  getChildDataKey ��      ̄      ��  R  \      CHARACTER,  getContainerHandle  ܄      �      <�  S  l      HANDLE, getContainerHidden  �      D�      x�  T        LOGICAL,    getContainerSource  X�      ��      ��  U  �      HANDLE, getContainerSourceEvents    ��      ��      ��  V  �      CHARACTER,  getContainerType    ܅      �      <�  W  �      CHARACTER,  getDataLinksEnabled �      H�      |�  X  �      LOGICAL,    getDataSource   \�      ��      ��  Y  �      HANDLE, getDataSourceEvents ��      ��      �  Z  �      CHARACTER,  getDataSourceNames  Ԇ       �      4�  [        CHARACTER,  getDataTarget   �      @�      p�  \        CHARACTER,  getDataTargetEvents P�      |�      ��  ]  &      CHARACTER,  getDBAware  ��      ��      �  ^ 
 :      LOGICAL,    getDesignDataObject ȇ      �      (�  _  E      CHARACTER,  getDynamicObject    �      4�      h�  `  Y      LOGICAL,    getInstanceProperties   H�      t�      ��  a  j      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  �      CHARACTER,  getObjectHidden �      <�      l�  d  �      LOGICAL,    getObjectInitialized    L�      x�      ��  e  �      LOGICAL,    getObjectName   ��      ��      �  f  �      CHARACTER,  getObjectPage   ̉      ��      (�  g  �      INTEGER,    getObjectParent �      4�      d�  h  �      HANDLE, getObjectVersion    D�      l�      ��  i  �      CHARACTER,  getObjectVersionNumber  ��      ��      �  j  	      CHARACTER,  getParentDataKey    Ċ      ��      $�  k         CHARACTER,  getPassThroughLinks �      0�      d�  l  1      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  E      CHARACTER,  getPhysicalVersion  ��      ��      �  n  [      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  n      CHARACTER,  getQueryObject  �      4�      d�  p  �      LOGICAL,    getRunAttribute D�      p�      ��  q  �      CHARACTER,  getSupportedLinks   ��      ��      ��  r  �      CHARACTER,  getTranslatableProperties   ��      �      (�  s  �      CHARACTER,  getUIBMode  �      4�      `�  t 
 �      CHARACTER,  getUserProperty @�      l�      ��  u  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z   	      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  .	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  ;	      CHARACTER,  setChildDataKey 4�      `�      ��  }  J	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  Z	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    m	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  �	      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  #
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  4
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  J
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  _
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  q
      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  �
      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �  �
      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �  �
      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �        LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 *      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  5      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  E      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 Q      CHARACTER,INPUT pcName CHARACTER    l�      ��  0�      �       4   �����                 @�                      ��                    L                  $�e	                         Ě           \�  ؛      �       4   �����                 �                      ��                  !  K                  ��e	                       !  l�  �    8  �  ��      �       4   �����                 ��                      ��                  D  F                  4�e	                       D  �         E                                  ,     
                    � ߱        �  $  H  ��  ���                           $  J  @�  ���                       x                         � ߱        x�    P  ��  �      �      4   �����                �                      ��                  Q  	                  ��e	                       Q  ��  H�  o   T      ,                                 ��  $   U  t�  ���                       �  @         �              � ߱        ��  �   V        Ȟ  �   W  �      ܞ  �   Y        �  �   [  x      �  �   ]  �      �  �   _  `      ,�  �   `  �      @�  �   a        T�  �   d  �      h�  �   f         |�  �   g  |      ��  �   i  �      ��  �   j  t      ��  �   k  �      ̟  �   l  ,      ��  �   m  �      ��  �   s  �      �  �   u  P	      �  �   {  �	      0�  �   }   
      D�  �     t
      X�  �   �  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  <	  j	  ��              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ P	  آ  ���                           O   h	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  Y                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  
                  ��e	                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    A
  T�  Ц      x      4   ����x                �                      ��                  B
  �
                  ��d	                       B
  d�  ��  �   D
  �      �  �   E
  T      �  �   F
  �      0�  �   G
  D      D�  �   H
  �      X�  �   I
  �      l�  �   K
  p      ��  �   L
  �      ��  �   M
  X      ��  �   N
  �      ��  �   O
  �      Ч  �   P
  D       �  �   Q
  �       ��  �   R
  �       �  �   S
  x!       �  �   T
  �!      4�  �   U
  h"      H�  �   V
  �"      \�  �   W
  `#      p�  �   X
  �#      ��  �   Y
  X$      ��  �   Z
  �$      ��  �   [
  �$      ��  �   \
  L%      Ԩ  �   ]
  �%      �  �   ^
  <&      ��  �   _
  �&      �  �   `
  4'      $�  �   a
  �'      8�  �   b
  ,(      L�  �   c
  h(      `�  �   e
  �(      t�  �   f
  X)      ��  �   g
  �)      ��  �   h
  *      ��  �   i
  �*      ĩ  �   j
  �*      ة  �   k
  l+      �  �   l
  �+       �  �   m
  \,      �  �   n
  �,      (�  �   o
  L-      <�  �   p
  �-      P�  �   q
  <.      d�  �   r
  �.      x�  �   s
  4/      ��  �   t
  �/          �   u
  $0      d�    �
  ��  8�      T0      4   ����T0                H�                      ��                  �
  ~                  4�e	                       �
  ̪  \�  �   �
  �0      p�  �   �
  (1      ��  �   �
  �1      ��  �   �
  2      ��  �   �
  �2      ��  �   �
  3      ԫ  �   �
  |3      �  �   �
  �3      ��  �   �
  t4      �  �   �
  �4      $�  �   �
  l5      8�  �   �
  �5      L�  �   �
  d6      `�  �      �6      t�  �     L7      ��  �     �7      ��  �     <8      ��  �     �8      Ĭ  �     ,9      ج  �     �9      �  �     :       �  �     X:      �  �   	  �:      (�  �   
  H;      <�  �     �;      P�  �     8<          �     �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  -                  <��                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
     �  O                  �f	                       �  <�  ̱  �   �  �K      $�  $  �  ��  ���                       �K     
                    � ߱        8�  �   �  L      ��  $   �  d�  ���                       ,L  @         L              � ߱        L�  $  �  ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V     �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $    x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   0  �  ���                                      ̵                      ��                  Q  �                  f	                       Q  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   f  �  ���                        adm-clone-props �  ��              �     W     `                          \  R                     start-super-proc    �  d�  �           �     X                                  s                     l�      �   �      �X      4   �����X      /     ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $  !  ��  ���                       @Y                         � ߱        ��    1  �  \�  ��  \Y      4   ����\Y                и                      ��                  2  6                  <f	                       2  �  pY                     �Y                     �Y                         � ߱            $  3  l�  ���                             7  �  T�      �Y      4   �����Y  �Y                         � ߱            $  8  (�  ���                       �Y                         � ߱        ع  $  <  ��  ���                       Ժ    ?  ��  �  \�  �Y      4   �����Y      $  @  0�  ���                       Z                         � ߱            �   ]  ,Z      lZ     
                �Z                     8\  @        
 �[              � ߱         �  V   q  p�  ���                        �  �   �  D\      �    #  0�  @�      �\      4   �����\      /   $  l�     |�                          3   �����\            ��                      3   �����\  �\     
                P]                     �^  @        
 `^              � ߱        ��  V   0  ��  ���                        �^     
                h_                     �`  @        
 x`              � ߱        ̼  V   T  <�  ���                        L�    �  �  d�      �`      4   �����`                t�                      ��                  �  �                  �e	                       �  ��  �  /   �  ��     ��                          3   �����`            н                      3   �����`      /   �  �     �                          3   ����a            <�                      3   ����8a  ��  /  A  x�         la                      3   ����Ta  initProps   x�  ��              ,     Y     $                             �  	                                   ̿          t�  \�      ��                   2  ��              �ef	                    O   ����    e�          O   ����    R�          O   ����    ��      �                      ��          ��  p   $  �t  �      /  t�  d�     �t                                        ��                  %  A                  �sf	                       %  ��   �  ��     �t                                        ��                  B  ^                  Ltf	                       B  ��  ��  |�     �t                                        ��                  _  {                  uf	                       _  �  �  �     u                                        ��                  |  �                  tnf	                       |  ��  ��  ��     u                                        ��                  �  �                  �nf	                       �  (�  0�   �     0u                                        ��                  �  �                  �of	                       �  ��  ��  ��     Du                                        ��                  �  �                  �pf	                       �  @�  H�  8�     Xu                                        ��                  �                    Xqf	                       �  ��  ��  ��     lu  	                                      ��             	       )                  �vf	                         X�  `�  P�     �u  
                                      ��             
     *  F                  <wf	                       *  ��  ��  ��     �u                                        ��                  G  c                  xf	                       G  p�  x�  h�     �u                                        ��                  d  �                  �xf	                       d  ��  �  ��     �u                                        ��                  �  �                  �yf	                       �  ��  ��  ��     �u                                        ��                  �  �                  �zf	                       �  �  �  �     �u                                        ��                  �  �                  �{f	                       �  ��  ��  ��     �u                                        ��                  �  �                  T|f	                       �  ,�  4�  $�     v                                        ��                  �                    $}f	                       �  ��      ��      v                                        ��                    .                  �}f	                         D�      O   1  ��  ��  4v               D�          ,�  8�   , �                                                       �     ��                            ����                            ��  d�  ��  �      ��     Z     L�                      � H�  �                     ��    G  �  ��      @v      4   ����@v                ��                      ��                  H  \                  �f	                       H  �  ��  /   I  ��     ��                          3   ����Pv            ��                      3   ����pv  h�  /   J  (�     8�                          3   �����v            X�                      3   �����v  ��  /   O  ��     ��                          3   �����v            ��                      3   �����v      /   U   �     �                          3   ����w            0�                      3   ����$w  Dw     
                �w                     y  @        
 �x              � ߱        ��  V   �  @�  ���                        ��  $  �  ��  ���                       $y                         � ߱        Dy     
                �y                     {  @        
 �z              � ߱        ��  V   �  (�  ���                        t�  $  �  ��  ���                       {     
                    � ߱        0{     
                �{                     �|  @        
 �|              � ߱        ��  V   �  �  ���                        \�  $    ��  ���                       }     
                    � ߱        }     
                �}                     �~  @        
 �~              � ߱        ��  V     ��  ���                        D�  $  2  ��  ���                                                 � ߱        (     
                �                     �  @        
 ��              � ߱        p�  V   <  ��  ���                        ��  �   V  �      @�  $  W  ��  ���                       ,�     
                    � ߱        @�     
                ��                     �  @        
 ̂              � ߱        l�  V   a  ��  ���                        ��  $  {  ��  ���                       �     
                    � ߱        ��  �   �  ,�      0�  $  �  �  ���                       l�     
                    � ߱        D�  �   �  ��      ��  $  �  p�  ���                       ��                         � ߱              �  ��  ��      ܃      4   ����܃      /   �  ��     �                          3   ������  4�     
   $�                      3   �����  d�        T�                      3   ����$�  ��        ��                      3   ����8�            ��                      3   ����T�  pushRowObjUpdTable  ��  ��  �                   [      �                               �                     pushTableAndValidate    ��  4�  �           |     \     �                          �                       remoteCommit    L�  ��  �           p     ]     �                          �  R                     serverCommit    ��  �  �           l     ^     �                          �  _                                     4�          �  ��      ��                  
    �              �f	                    O   ����    e�          O   ����    R�          O   ����    ��          O     ��  ��  ��    ��                            ����                            $�  P�      ��              _      L�                      
�     l                     disable_UI  ��  ��                      `      �                                 
                    �  �    ����  �       ��       ��  8   ����   ��  8   ����       8   ����       8   ����       ��  ��      viewObject  ,   ��  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  �  4�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  �  x�  ��      returnFocus ,INPUT hTarget HANDLE   h�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  ��  �      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  \�  l�      removeAllLinks  ,   L�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE p�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  t�  ��      hideObject  ,   d�  ��  ��      exitObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  ��  ��      displayLinks    ,   ��  �  �      createControls  ,   ��  (�  8�      changeCursor    ,INPUT pcCursor CHARACTER   �  d�  p�      applyEntry  ,INPUT pcField CHARACTER    T�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  �  �      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  h�  p�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE X�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  �      runServerObject ,INPUT phAppService HANDLE  ��  8�  L�      disconnectObject    ,   (�  `�  p�      destroyObject   ,   P�  ��  ��      bindServer  ,   t�  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  ��  �      startFilter ,   ��  �  (�      releaseDBRow    ,   �  <�  L�      refetchDBRow    ,INPUT phRowObjUpd HANDLE   ,�  x�  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE h�  ��  ��      fetchDBRowForUpdate ,   ��  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  (�  8�      compareDBRow    ,   �  L�  `�      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   <�  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  �  �      updateState ,INPUT pcState CHARACTER    ��  D�  X�      updateQueryPosition ,   4�  l�  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    \�  ��  ��      undoTransaction ,   ��  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  x�  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   h�  ��  �      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  P�  d�      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  @�  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  ,�  @�      startServerObject   ,   �  T�  d�      setPropertyList ,INPUT pcProperties CHARACTER   D�  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ��   �      rowObjectState  ,INPUT pcState CHARACTER    ��  ,�  <�      retrieveFilter  ,   �  P�  d�      restartServerObject ,   @�  x�  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   h�  ��  ��      refreshRow  ,   p�  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  �  $�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  �  T�  l�      initializeServerObject  ,   D�  ��  ��      initializeObject    ,   p�  ��  ��      home    ,   ��  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��   �  �      fetchPrev   ,   ��   �  ,�      fetchNext   ,   �  @�  L�      fetchLast   ,   0�  `�  l�      fetchFirst  ,   P�  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   p�  ��  ��      endClientDataRequest    ,   ��  ��  ��      destroyServerObject ,   ��  �  �      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    ��  h�  x�      dataAvailable   ,INPUT pcRelative CHARACTER X�  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  ��  �      commitTransaction   ,   ��   �  0�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    �  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        ��    :   %               � 
"    
 �%              h �P  \         (          
�                          
�            � [   u	
"    
 e	
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 ��               1� k  
 �� v   �%               o%   o           � {    �
"   
 ��           �    1� |   �� v   �%               o%   o           � �   �
"   
 ��           �    1� �  
 �� v   �%               o%   o           � �   �
"   
 ��           l    1� �   �� v   �%               o%   o           � {    �
"   
 ��           �    1� �   �� v   �%               o%   o           � �   �
"   
 ��           T    1� �   �� �   �%               o%   o           %               
"   
 ��          �    1� �   ��      
"   
 ��               1� 	   �� v   �%               o%   o           �   �
"   
 ��           �    1�    �� v   �%               o%   o           � -  S �
"   
 ��           �    1� �   �� �   �%               o%   o           %               
"   
 ��           p    1� �   �� �   �%               o%   o           %               
"   
 ��           �    1� �   �� �   �%               o%   o           %              
"   
 ��          h    1� �   �� �     
"   
 ��           �    1� �  
 �� �   �%               o%   o           %               
"   
 ��                1� �   �� v   �%               o%   o           � {    �
"   
 ��          �    1� �   ��      
"   
 ��           �    1� �   �� v   �%               o%   o           � �  t �
"   
 ��          D	    1� m  
 ��      
"   
 ��           �	    1� x   �� v   �%               o%   o           � �  � �
"   
 ��           �	    1�    �� v   �%               o%   o           � {    �
"   
 ��           h
    1� -  
 �� 8   �%               o%   o           %               
"   
 e	�           �
    1� <   e	� �   �%               o%   o           %              
"   
 e	�           `    1� D   e	� v   �%               o%   o           � {    e	
"   
 e	�           �    1� U   e	� v   �%               o%   o           o%   o           
"   
 e	�           P    1� e  
 e	� v   �%               o%   o           � {    e	
"   
 e	�           �    1� p   e	� �  	 �%               o%   o           � �  / e	
"   
 ��          8    1� �   �� �  	   
"   
 e	�           t    1� �   e	� �  	 �o%   o           o%   o           � {    e	
"   
 ��          �    1� �   �� �  	   
"   
 e	�           $    1� �   e	� �  	 �o%   o           o%   o           � {    e	
"   
 ��          �    1� �   �� �     
"   
 ��          �    1�    �� �  	   
"   
 ��              1�    �� �  	   
"   
 ��          L    1� '   �� �  	   
"   
 e	�           �    1� 5   e	� �   �o%   o           o%   o           %              
"   
 ��              1� F   �� �  	   
"   
 ��          @    1� T  
 �� _     
"   
 ��          |    1� g   �� �  	   
"   
 ��          �    1� v   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          0    1� �   �� �  	   
"   
 ��          l    1� �  	 �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 e	�                1� �   e	� v   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 e	
"   
   
"   
 u	(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�            �� �     p�               �L
�    %              � 8          � $         � �          
�    �      
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 e	�           �    1�   
 e	� v   �%               o%   o           � {    e	
"   
 e	�           <    1� %  
 e	� v   �%               o%   o           o%   o           
"   
 e	�           �    1� 0   e	�    �%               o%   o           o%   o           
"   
 e	�           4    1� 9   e	� �   �%               o%   o           %               
"   
 e	�           �    1� H   e	� �   �%               o%   o           %               
"   
 ��           ,    1� U   �� v   �%               o%   o           � {    e	
"   
 e	�           �    1� \   e	� �   �%               o%   o           %              
"   
 e	�               1� n   e	� �   �%               o%   o           o%   o           
"   
 e	�           �    1� z   e	� v   �%               o%   o           o%   o           
"   
 e	�               1� �  	 e	� v   �%               o%   o           � {    e	
"   
 e	�           �    1� �   e	� v   �%               o%   o           o%   o           
"   
 e	�               1� �   e	� v   �%               o%   o           o%   o           
"   
 e	�           �    1� �   e	� �   �%               o%   o           %               
"   
 e	�           �    1� �   e	� �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 e	�           �    1� �  
 e	� �   �%               o%   o           %              
"   
 e	�           H    1� �   e	� v   �%               o%   o           o%   o           
"   
 e	�           �    1� �   e	� v   �%               o%   o           � {    e	
"   
 e	�           8    1� �   e	� v   �%               o%   o           o%   o           
"   
 ��          �    1�    ��      
"   
 e	�           �    1�    e	� v   �%               o%   o           � "  ! e	
"   
 e	�           d    1� D   e	� v   �%               o%   o           � {    e	
"   
 e	�           �    1� Q   e	� v   �%               o%   o           � d   e	
"   
 ��          L    1� s   �� �     
"   
 ��          �    1� �   ��      
"   
 e	�           �    1� �   e	� v   �%               o%   o           � {    e	
"   
 ��          8     1� �  
 ��      
"   
 ��           t     1� �   �� �   �%               o%   o           o%   o           
"   
 e	�           �     1� �   e	� �   �%               o%   o           %               
"   
 e	�           l!    1� �   e	� �   �%               o%   o           %               
"   
 e	�           �!    1� �   e	� v   �%               o%   o           � {    e	
"   
 e	�           \"    1� �   e	� v   �%               o%   o           o%   o           
"   
 e	�           �"    1� �   e	� �   �%               o%   o           %              
"   
 e	�           T#    1� 
   e	� �   �%               o%   o           %               
"   
 ��           �#    1�    �� �   �%               o%   o           %               
"   
 ��          L$    1� '   ��      
"   
 ��          �$    1� 4   �� v     
"   
 e	�           �$    1� A   e	� 8   �%               o%   o           o%   o           
"   
 e	�           @%    1� M   e	� v   �%               o%   o           � {    e	
"   
 e	�           �%    1� [   e	� v   �%               o%   o           o%   o           
"   
 e	�           0&    1� i   e	� �   �o%   o           o%   o           o%   o           
"   
 e	�           �&    1� ~   e	� �  	 �%               o%   o           o%   o           
"   
 e	�           ('    1� �   e	� v   �%               o%   o           o%   o           
"   
 e	�           �'    1� �  
 e	� 8   �%               o%   o           o%   o           
"   
 ��           (    1� �   �� v     
"   
 e	�           \(    1� �   e	� v   �%               o%   o           � �  4 e	
"   
 e	�           �(    1�   
 e	� �   �%               o%   o           %              
"   
 ��          L)    1�    ��      
"   
 e	�           �)    1�     e	� v   �%               o%   o           � {    �
"   
 e	�           �)    1� .   e	� �   �%               o%   o           %              
"   
 e	�           x*    1� =   e	� v   �%               o%   o           � {    e	
"   
 e	�           �*    1� J   e	� v   �%               o%   o           � {    e	
"   
 e	�           `+    1� X   e	� v   �%               o%   o           � {    e	
"   
 e	�           �+    1� d   e	� �   �%               o%   o           %               
"   
 e	�           P,    1� s  	 e	�    �%               o%   o           o%   o           
"   
 ��           �,    1� }   �� v   �%               o%   o           � �  	 e	
"   
 e	�           @-    1� �   e	� 8   �%               o%   o           %       �       
"   
 e	�           �-    1� �   e	� v   �%               o%   o           � {    e	
"   
 e	�           0.    1� �   e	� �   �o%   o           o%   o           %              
"   
 e	�           �.    1� �   e	� �   �%               o%   o           %               
"   
 e	�           (/    1� �   e	� v   �%               o%   o           o%   o           
"   
 e	�           �/    1� �   e	� �  	 �%               o%   o           � {    e	
"   
 ��          0    1� �   �� �  	   P �L 
�H T   %              �     }        �GG %              
"   
 ��           �0    1�   
 �� v   �%               o%   o           � {    �
"   
 e	�           1    1�    e	� �   �%               o%   o           %               
"   
 e	�           �1    1�   	 e	� v   �%               o%   o           � {    e	
"   
 e	�           2    1� #   e	� v   �%               o%   o           � {    e	
"   
 e	�           �2    1� 1   e	� �   �%               o%   o           %               
"   
 e	�           �2    1� A   e	� v   �%               o%   o           � {    e	
"   
 e	�           p3    1� T   e	� v   �%               o%   o           o%   o           
"   
 e	�           �3    1� \   e	� v   �%               o%   o           o%   o           
"   
 e	�           h4    1� i   e	� �   �%               o%   o           o%   o           
"   
 ��           �4    1� w   �� �   �%               o%   o           o%   o           
"   
 e	�           `5    1� �   e	� �   �%               o%   o           o%   o           
"   
 e	�           �5    1� �   e	� v   �%               o%   o           o%   o           
"   
 e	�           X6    1� �  	 e	� �  	 �%               o%   o           � {    e	
"   
 e	�           �6    1� �  
 e	� �  	 �%               o%   o           � {    e	
"   
 e	�           @7    1� �   e	� v   �%               o%   o           � {    e	
"   
 e	�           �7    1� �   e	� v   �%               o%   o           o%   o           
"   
 e	�           08    1� �   e	� v   �%               o%   o           o%   o           
"   
 e	�           �8    1� �   e	� v   �%               o%   o           � {    e	
"   
 e	�            9    1� �   e	� v   �%               o%   o           � {    e	
"   
 e	�           �9    1� 
   e	� �  	 �%               o%   o           o%   o           
"   
 ��          :    1�    ��      
"   
 e	�           L:    1� (   e	� v   �%               o%   o           � {    e	
"   
 e	�           �:    1� 6   e	� v   �%               o%   o           o%   o           
"   
 ��           <;    1� I   �� �   �%               o%   o           o%   o           
"   
 e	�           �;    1� [  
 e	� v   �%               o%   o           � {    e	
"   
 e	�           ,<    1� f   e	� v   �%               o%   o           � {    e	
"   
 e	�           �<    1� ~   e	� �   �%               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 e	�           p=    1� �  	 e	�    �%               o%   o           o%   o           
"   
 e	�           �=    1� �   e	�    �%               o%   o           o%   o           
"   
 e	�           h>    1� �   e	�    �%               o%   o           o%   o           
"   
 ��           �>    1� �   �� �   �%               o%   o           %              
"   
 e	�           `?    1� �   e	� v   �%               o%   o           � �  M �
"   
 e	�           �?    1� 2   e	� �   �%               o%   o           %              
"   
 e	�           P@    1� C   e	� �   �%               o%   o           %               
"   
 e	�           �@    1� W   e	� �   �%               o%   o           %               
"   
 e	�           HA    1� n   e	� �  	 �%               o%   o           � |   e	
"   
 e	�           �A    1� �   e	� �   �%               o%   o           %               
"   
 e	�           8B    1� �   e	� �  	 �%               o%   o           o%   o           
"   
 e	�           �B    1� �   e	� �   �o%   o           o%   o           %              
"   
 ��           0C    1� �   �� �  	 �o%   o           o%   o           � {    �
"   
 e	�           �C    1� �   e	�    �o%   o           o%   o           o%   o           
"   
 e	�            D    1� �   e	�    �o%   o           o%   o           o%   o           
"   
 e	�           �D    1� �   e	� �  	 �o%   o           o%   o           o%   o           
"   
 e	�           E    1� �   e	�    �o%   o           o%   o           o%   o           
"   
 e	�           �E    1�    e	� �  	 �o%   o           o%   o           �    e	
"   
 e	�           F    1�    e	� �  	 �o%   o           o%   o           � +   e	
"   
 e	�           |F    1� 7   e	� �   �%               o%   o           %               
"   
 e	�           �F    1� K   e	� �   �%               o%   o           %               
"   
 ��          tG    1� _   �� �  	   
"   
 e	�           �G    1� s   e	� �   �%               o%   o           %               
"   
 e	�           ,H    1�    e	� v   �%               o%   o           o%   o           
"   
 e	�           �H    1� �   e	� v   �%               o%   o           o%   o           
"   
 e	�           $I    1� �   e	� �   �%               o%   o           o%   o           
"   
 e	�           �I    1� �   e	� v   �%               o%   o           � {    e	
"   
 e	�           J    1� �   e	� �   �%               o%   o           %               
"   
 e	�           �J    1� �  	 e	� �   �%               o%   o           %                "    �%     start-super-proc v�%     adm2/smart.p �u	P �L 
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
 u	
"   
 �
"   
 u	
"   
   (�  L ( l       �        �M    �� �   � P   �        �M    �@    
� @  , 
�       �M    �� �   u	p�               �L
�    %              � 8       N    � $         � �          
�    �    u	
"   
 �p� @  , 
�       O    �� 	   �p�               �L"  	  , �   �    e	�    ��     }        �A      |    "  	    �    e	%              (<   \ (    |    �     }        �A�    �A"  
  e	    "  	  u	"  
  e	  < "  	  u	"  
  e	(    |    �     }        �A�    �A"  
  e	
�H T   %              �     }        �GG %              
"   
 u	
"   
 �
"   
 u	
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   u	p�               �L
�    %              � 8      Q    � $         � �          
�    �    u	
"   
 �p� @  , 
�       R    �� k  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 f	
"   
   
"   
   (�  L ( l       �        �R    �� �   � P   �        �R    �@    
� @  , 
�       �R    �� �     p�               �L
�    %              � 8      �R    � $         � �          
�    �      
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
�       U    �� �    p�               �L(        � {      � {      � {      �     }        �A
�H T   %              �     }        �GG %              
"   
 e	 (   � 
"   
 u	    �        �U    �� �   �
"   
   � 8      DV    � $         � �          
�    �    u	
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
   p�    � A   e	
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 u	    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 e	"      �       }        �
"   
 �%              %                "    �%     start-super-proc u�%     adm2/appserver.p �e	�    � �     
�    �     }        �%               %      Server  - �     }        �    "    e	� {    �%               %      Client      "    e	� {    �%      NONE    p�,  8         $     "    e	        � �   u	
�    
�H T   %              �     }        �GG %              
"   
 u	
"   
 �
"   
 u	
"   
   (�  L ( l       �        �Z    �� �   � P   �        �Z    �@    
� @  , 
�       �Z    �� �   u	p�               �L
�    %              � 8      �Z    � $         � �          
�    �    u	
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "    e	        � �   u	
�     "    �%     start-super-proc t�%     adm2/dataquery.p �e	
�H T   %              �     }        �GG %              
"   
 u	
"   
 �
"   
 u	
"   
 u	(�  L ( l       �         ]    �� �   � P   �        ,]    �@    
� @  , 
�       8]    �� �   u	p�               �L
�    %              � 8      D]    � $         � �   u	     
�    �    u	
"   
 �p� @  , 
�       T^    �� x   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 u	
"   
 �
"   
 u	
"   
 u	(�  L ( l       �        8_    �� �   � P   �        D_    �@    
� @  , 
�       P_    �� �   u	p�               �L
�    %              � 8      \_    � $         � �   u	     
�    �    u	
"   
 �p� @  , 
�       l`    �� �   �p�               �L%               "    �%     start-super-proc t�%     adm2/query.p �u	%     start-super-proc t�%     adm2/queryext.p % 	    initProps u	
�    %4 + $   FOR EACH Almcmov NO-LOCK INDEXED-REPOSITION �   � ^     � `     � b     
�H T   %              �     }        �GG %              
"   
 u	
"   
 �
"   
 u	
"   
   (�  L ( l       �        $b    �� �   � P   �        0b    �@    
� @  , 
�       <b    �� �   u	p�               �L
�    %              � 8      Hb    � $         � �          
�    �    u	
"   
 �p� @  , 
�       Xc    �� �   �p�               �L"    ,     "    e	� j    �
�H T   %              �     }        �GG %              
"   
 u	
"   
 �
"   
 u	
"   
   (�  L ( l       �        d    �� �   � P   �        (d    �@    
� @  , 
�       4d    �� �   u	p�               �L
�    %              � 8      @d    � $         � �          
�    �    u	
"   
 �p� @  , 
�       Pe    ��   	 �p�               �L"    , %               �    "      � `         %              %                   "      %                  "      "      "     T(        "    f	%              "    f	� `   �"      �       "    u	�    "    f	�    �� {      �    u	�    "     �     S    "      "    �    "    f	%                � @    �     t T     P   4       �"      (0       4       f	"      � {      � {    u	� ^   f	T ,  %              T   "    f	"    �� `     �    u	� ^   f	T    �    "    f	�    �"      �    u	"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    u	%              � {    �� k     4  f	     "      
�H T   %              �     }        �GG %              
"   
 u	
"   
 �
"   
 u	
"   
   (�  L ( l       �        j    �� �   � P   �        $j    �@    
� @  , 
�       0j    �� �   u	p�               �L
�    %              � 8      <j    � $         � �          
�    �    u	
"   
 �p� @  , 
�       Lk    ��   
 �p�               �L"    ,       "  
  e	�    � m  " e	� `   �      "  	    �    � m  " �� `   e	�   � ^     � `     � m  " u	�   � ^     � `   u	� m  " e	�   � ^     � `     � m  "   
�H T   %              �     }        �GG %              
"   
 �
"   
 u	
"   
 �
"   
 �(�  L ( l       �        �l    �� �   � P   �        �l    �@    
� @  , 
�       �l    �� �   �p�               �L
�    %              � 8       m    � $         � �          
�    �      
"   
 �p� @  , 
�       n    �� �   �p�               �L"    , 
"   
   p� @  , 
�       hn    �� f     p�               �L"    , 
"   
  p� @  , 
�       �n    �� A    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � m  "   � `         "  	  f	�     "    f	T    "      "      @ A,    �   � ^   �� k     "    u	"       T      @   "    �(        "      � {    u	� {      � ^   u	"    f	     "  	   %              D H   @ A,    �   � ^   u	� k     "    u	"    e	,    S   "    u	� m  " e	� `   �%                T      @   "    �(        "      � {    u	� {      � ^   u	"    e	     "  
   %                         "    �� k     "    u	           "      � k   u	"      
�H T   %              �     }        �GG %              
"   
 f	
"   
   
"   
 f	
"   
 u	(�  L ( l       �        �r    �� �   � P   �        �r    �@    
� @  , 
�       �r    �� �   f	p�               �L
�    %              � 8       s    � $         � �   u	     
�    �    �
"   
 �p� @  , 
�       t    �� f   �p�               �L"    , 
"   
   p� @  , 
�       ht    �� A     p�               �L"    , "      %              %              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    �%     start-super-proc s�%     adm2/data.p %     start-super-proc s�%     adm2/dataext.p 	%     start-super-proc s�%     adm2/dataextcols.p 	%     start-super-proc s�%     adm2/dataextapi.p f	
�H T   %              �     }        �GG %              
"   
 u	
"   
 �
"   
 u	
"   
 u	(�  L ( l       �        �w    �� �   � P   �        �w    �@    
� @  , 
�       �w    �� �   u	p�               �L
�    %              � 8      �w    � $         � �   u	     
�    �    u	
"   
 �p� @  , 
�       �x    �� s   �p�               �L%               %     "Util/dalmcmov.i"   
�H T   %              �     }        �GG %              
"   
 u	
"   
 �
"   
 u	
"   
   (�  L ( l       �        �y    �� �   � P   �        �y    �@    
� @  , 
�       �y    �� �   u	p�               �L
�    %              � 8      �y    � $         � �          
�    �    u	
"   
 �p� @  , 
�       �z    �� n   �p�               �L"    , 
�     	         �G
�H T   %              �     }        �GG %              
"   
 u	
"   
 �
"   
 u	
"   
   (�  L ( l       �        |{    �� �   � P   �        �{    �@    
� @  , 
�       �{    �� �   u	p�               �L
�    %              � 8      �{    � $         � �          
�    �    u	
"   
 �p� @  , 
�       �|    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 u	
"   
 �
"   
 u	
"   
   (�  L ( l       �        h}    �� �   � P   �        t}    �@    
� @  , 
�       �}    �� �   u	p�               �L
�    %              � 8      �}    � $         � �          
�    �    u	
"   
 �p� @  , 
�       �~    �� s  	 �p�               �L
"   
 , 
"   
 �     � �  	   �        �~    �
�H T   %              �     }        �GG %              
"   
 u	
"   
 �
"   
 u	
"   
   (�  L ( l       �        t    �� �   � P   �        �    �@    
� @  , 
�       �    �� �   u	p�               �L
�    %              � 8      �    � $         � �          
�    �    u	
"   
 �p� @  , 
�       ��    �� �   �p�               �L"    , 
"   
   �        �    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 u	
"   
 �
"   
 u	
"   
   (�  L ( l       �        ��    �� �   � P   �        ��    �@    
� @  , 
�       ��    �� �   u	p�               �L
�    %              � 8      ��    � $         � �          
�    �    u	
"   
 �p� @  , 
�       ��    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 f	        � �   u	
�    
�             �Gp�,  8         $     
"   
 f	        � �   u	
�    �    � �     
�        "    �� {    �%     modifyListProperty 
�    %      REMOVE  %     SupportedLinks 	%     Update-Target  	%     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � ;     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   l       ��                 ^  �  �               \
f	                    O   ����    e�          O   ����    R�          O   ����    ��        $  m  �   ���                       �U     
                    � ߱              n  (  �      V      4   ����V                �                      ��                  o  �                  7f	                       o  8  �  �  p  PV            r  �  `      �V      4   �����V                p                      ��                  s  �                  p7f	                       s  �  �  o   t      ,                                 �  �   u  �V      �  �   v  �V      $  $  w  �  ���                        W     
                    � ߱        8  �   x  @W      L  �   y  `W      `  �   |  �W          $     �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               �8f	                    O   ����    e�          O   ����    R�          O   ����    ��      b                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  ,=f	                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��                 M    �               �e	                    O   ����    e�          O   ����    R�          O   ����    ��        $  m  �   ���                       ta                         � ߱        �  $  n  8  ���                       �a                         � ߱        �a     
                Tb                     �c  @        
 dc              � ߱        �  V   �  d  ���                        �    �    �      �c      4   �����c  �c     
                Ld                     �e  @        
 \e              � ߱            V   �     ���                          $    �  ���                       �e                         � ߱        �  $    4  ���                       �e                         � ߱          �      4  8                      ��        0           1                  �Yf	      Tf     �       `      $      ���                       �e                         � ߱        �  $    `  ���                       f                         � ߱            4   ����4f  `f                     �f                     �f                     g                     $g                         � ߱        d  $    �  ���                             )  �  �      Dg      4   ����Dg      $  *  �  ���                       lg          �h             � ߱        �  $  4    ���                       �h                         � ߱          �        |                      ��        0         6  ;                  ��e	      8i     8     6  @      $  6  �  ���                       �h                         � ߱        l  $  6  @  ���                       �h                         � ߱            4   ����i      $  8  �  ���                       Li                         � ߱        �i     
                Hj                     �k  @        
 Xk              � ߱        �  V   F  �  ���                        �k       
       
       �k       	       	       l                     8l                         � ߱        	  $  �  d  ���                       
  $  1  <	  ���                       dl                         � ߱        �l     
                m                     \n  @        
 n          �n  @        
 tn          o  @        
 �n              � ߱        �
  V   =  h	  ���                          �
        |                      ��        0         �  �                  Dcf	      �o     T     �  0
      $  �  �
  ���                       o                         � ߱        \  $  �  0  ���                       Ho                         � ߱        l  4   ����po      4   �����o  �  $  �  �  ���                       p                         � ߱        �    �  �  l      0p      4   ����0p                �                      ��                  �  �                  �cf	                       �     tp                     �p       	       	           � ߱            $  �  |  ���                             �    �      q      4   ����q                �                      ��                  �  �                  ddf	                       �    �q                      r       
       
           � ߱            $  �  �  ���                       (r                     \r                         � ߱           $  �    ���                       �r     
                s                     \t  @        
 t          �t  @        
 tt              � ߱            V   �  �  ���                                    7 �          �  d  � `                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             �   �   �   �   �   �   �   �       $  4  D  T  d  t  �  \  �  �  �  �  8HXhx��������(8HX      �   �   �   �   �   �   �   �      $  4  D  T  d  t  �   \ �  �  �  �  8HXhx��������(8HX  �                    � �                     �          ��                            ����                                                        �   l       ��                  y  �  �               ��f	                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                  �  �  �               �f	                    O   ����    e�          O   ����    R�          O   ����    ��      �       �              �                  $                  d  /  �  $     4  ��                      3   ����p�            T                      3   ������      O   �  ��  ��  ��               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  �  �  �               ��f	                    O   ����    e�          O   ����    R�          O   ����    ��             �              �                $                  &       ,             �          1                                �  /  �  t     �  Ȅ                      3   ������            �                      3   ����Є     /  �  �     �  ��                      3   ����܄  x                             3   ���� �      $   �  L  ���                                                   � ߱                  �  �                  3   �����      $   �  �  ���                                                   � ߱        X  $  �  ,  ���                       �                         � ߱            O   �  ��  ��  4�               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                  �  �  �               ��f	                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  &                    �          1                      �              /  �  L     \  d�                      3   ����H�  �        |  �                  3   ����l�      $   �  �  ���                                                   � ߱                                      3   ����x�      $   �  @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                  V  a  �               ��f	                    O   ����    e�          O   ����    R�          O   ����    ��            `  �   �       ��      4   ������      �   `  ��    ��                            ����                            TXS appSrvUtils O:\on_in_co\Util\dalmcmov.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "Util/dalmcmov.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server Client NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH Almcmov NO-LOCK INDEXED-REPOSITION ,   Almcmov  ; TipMov CodMov NroSer NroDoc FchDoc INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p TipMov CodMov NroSer NroDoc FchDoc RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery x  �-  �  H;      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   P	  h	  j	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props m  n  o  p  r  s  t  u  v  w  x  y  |    �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable             |     cQueryString    |  �  "   Y   �          �                  initProps   m  n  �  �  �          )  *  1  4  6  8  ;  F  �  1  =  �  �  �  �  �  �  �  �  �  �  �  �  �              d     lRet              �        piTableIndex    �  �  (   Z   P  l      �                  deleteRecordStatic  $  %  A  B  ^  _  {  |  �  �  �  �  �  �  �  �      )  *  F  G  c  d  �  �  �  �  �  �  �  �  �  �      .  /  1  2                 !       �  �     [       x      �                  pushRowObjUpdTable  �  �        �        pcValType                  $       �  \     \       �      D                  pushTableAndValidate    �  �  �  �        �        pcContext   �             $       �        �        pcMessages            �        pcUndoIds     ,     ]       h                        remoteCommit    �  �  �  �  �  X             $       |        p        pcMessages            �        pcUndoIds   �  �     ^       @      �                  serverCommit    �  �  �  ,     _                                 getRowObjUpdStatic      �  p     `               d                  disable_UI  `  a  4  �       �      �                      �  �  �  
   RowObject   <         D         L         T         \         d         l         x         �         �         TipMov  CodMov  NroSer  NroDoc  FchDoc  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �  �     RowObjUpd   8         @         H         P         X         `         h         t         |         �         �         TipMov  CodMov  NroSer  NroDoc  FchDoc  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   �          �  
   appSrvUtils �       �     xiRocketIndexLimit             
   gshAstraAppserver   <        (  
   gshSessionManager   `        P  
   gshRIManager    �        t  
   gshSecurityManager  �        �  
   gshProfileManager   �        �  
   gshRepositoryManager      	 	     �  
   gshTranslationManager   ,  
 
       
   gshWebManager   P        @     gscSessionId    t        d     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager           �     gsdTempUniqueID $             gsdUserObj  L        8     gsdRenderTypeObj    t        `     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf   �       �     glADMLoadFromRepos              glADMOk 8       ,  
   ghContainer X    	   L     cObjectName t    
   l     iStart  �       �     cAppService �       �     cASDivision �       �     cServerOperatingMode           �     cContainerType  (            cQueryString    H       <  
   hRowObject  h       \  
   hDataQuery  �       |     cColumns             �     cDataFieldDefs  �    X  �  RowObject         X  �  RowObjUpd          "   >   �   �   �   �          !  8  D  E  F  H  J  K  L  P  Q  T  U  V  W  Y  [  ]  _  `  a  d  f  g  i  j  k  l  m  s  u  {  }    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  A
  B
  D
  E
  F
  G
  H
  I
  K
  L
  M
  N
  O
  P
  Q
  R
  S
  T
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
                     	  
        ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  -  �  �  �  �  �  �  �  �  �      0  O  Q  f  �      !  1  2  3  6  7  8  <  ?  @  ]  q  �  #  $  0  T  �  �  �  �  �  A  G  H  I  J  O  U  \  �  �  �  �  �      2  <  V  W  a  {  �  �  �  �  �  �      <d  O:\on_in_co\Util\dalmcmov.w  #  ��  C:\Progress\OpenEdge\src\adm2\data.i 8#  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    h#  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i �#  �� , O:\on_in_co\Util\dalmcmov.i  �#  �:   C:\Progress\OpenEdge\src\adm2\query.i    $  z + C:\Progress\OpenEdge\src\adm2\delrecst.i 8$  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  l$   ) %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   �$  � ! C:\Progress\OpenEdge\src\adm2\dataquery.i    �$  �Z ( %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   %  �< " C:\Progress\OpenEdge\src\adm2\appserver.i    \%  �� ' %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �%  I� # C:\Progress\OpenEdge\src\adm2\smart.i    �%  Ds & C:\Progress\OpenEdge\gui\fn  &  tw % %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   4&  Q. $ C:\Progress\OpenEdge\gui\set t&  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i �&  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    �&  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    '  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  X'  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i �'  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i �'   
 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    (  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   H(  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   �(  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �(  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    )  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    P)  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �)  �j  C:\Progress\OpenEdge\gui\get �)  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �)  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    4*  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i x*  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �*  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �*  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i    +  �  C:\Progress\OpenEdge\src\adm2\appsprto.i d+  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �+  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �+  ª  %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   ,  �� 	 C:\Progress\OpenEdge\src\adm2\qryprto.i  d,  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   �,  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �,  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    -  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   T-  ��    O:\on_in_co\Util\dalmcmov_cl.w          g      �-  �        �-  [  �     �-     �  &   �-  �   E     �-     �  .   .  �   �     .     �     (.  �   �     8.     �  $   H.  �   �     X.     }  $   h.  �   {     x.     Y  $   �.  �   V     �.     4  $   �.  �   2     �.       $   �.  �        �.     �  $   �.  �   �     �.     �  $   /  �   �     /     �  $   (/  �   �     8/     ~  -   H/  �   z     X/     t  ,   h/  k   :     x/  �  .      �/       +   �/  �        �/     �  +   �/  �  �      �/     �  +   �/  �  �      �/     �  +   �/  �  �      0     �  +   0  �  �      (0     �  +   80  �  �      H0     f  +   X0  �  c      h0     I  +   x0  �  F      �0     ,  +   �0  �  )      �0       +   �0  �        �0     �  +   �0  �  �      �0     �  +   �0  �  �      1     �  +   1  �  �      (1     �  +   81  �  �      H1     ~  +   X1  �  {      h1     a  +   x1  �  ^      �1     D  +   �1  �  A      �1     '  +   �1  �        �1     �  $   �1  �  �      �1     �  $   �1  k  �      2     {  $   2  j  z      (2     X  $   82  i  W      H2     5  $   X2  _  +      h2       *   x2  ^        �2     �  *   �2  ]  �      �2     �  *   �2  \  �      �2     �  *   �2  [  �      �2     i  *   �2  Z  h      3     B  *   3  Y  A      (3       *   83  X        H3     �  *   X3  W  �      h3     �  *   x3  V  �      �3     �  *   �3  U  �      �3       *   �3  T  ~      �3     X  *   �3  S  W      �3     1  *   �3  R  0      4     
  *   4  Q  	      (4     �  *   84  P  �      H4     �  *   X4  O  �      h4     �  *   x4  N  �      �4     n  *   �4  @  `      �4     >  $   �4          �4     �  $   �4    �      �4     �  $   �4  �   =      5     �  )   5  g   �      (5  a   �  !   85     p  (   H5  _   n  !   X5     L  $   h5  ]   J  !   x5     (  $   �5  I     !   �5  �     "   �5     �  '   �5  �   �  "   �5     �  $   �5  �   �  "   �5     i  $   �5  �   g  "   6     E  $   6  g   +  "   (6          86  O   �  "   H6  �   ~  #   X6     |  &   h6  �   L  #   x6     �  %   �6  �   �  #   �6     �  $   �6  �   �  #   �6     �  $   �6  �   �  #   �6     �  $   �6  �   �  #   �6     ^  $   7  �   J  #   7     (  $   (7  }     #   87     �  $   H7     ~  #   X7     0  "   h7     �  !   x7     �      �7     6     �7  �   -     �7  O        �7          �7     �     �7  �   �     �7  �   ~     �7  O   p     8     _     8          (8  y   �
     88  �   �
  
   H8  G   �
     X8     �
     h8     y
     x8  c   
  
   �8  x   
     �8  M   �	     �8     �	     �8     �	     �8  a   �	     �8  �  g	     �8     H	     �8  �  	     9  O   	     9     �     (9     �     89  �   �     H9     �     X9     �     h9  x   �     x9     �     �9     c     �9     _     �9     K     �9     2     �9  Q   "     �9     �     �9     �     �9     |     :     b     :  ]   \  
   (:     R     8:     
  
   H:     �     X:     �  
   h:  Z   �     x:     �  	   �:     �     �:     �     �:     �     �:  c   f     �:     D     �:     �      �:     �      �:     �      ;     �      ;     &      (;           8;           