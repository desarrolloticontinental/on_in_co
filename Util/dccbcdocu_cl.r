	��V(8�K�6   �                                              & 36A800EFutf-8 MAIN O:\on_in_co\Util\dccbcdocu_cl.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,NroDoc character 0 0,RowNum integer 1 0,RowIdent character 2 0,RowMod character 3 0,RowIdentIdx character 4 0,RowUserProp character 5 0,ChangedFields character 6 0     4              D             n� 4  �              ��              �:     +   �� �  W   �� `  X   �� d  Y   `�   [   p�   \   �� <  ]   ī    ^   � 0  `   ? � Z  iSO8859-1                                                                           �    �                                      �                   ��                �  �       h5   T�              ��  �   �                                                               PROGRESS                         �           
    
                    �              �                                                                                                     
  H  �      �  
    
                  �  x             4                                                                                          �          
  �  �      p  
    
                  \  $             �                                                                                          �          
  �          
    
                    �             �                                                                                                    
  L        �  
    
                  �  |             8                                                                                                    
  �  '      t  
    
                  `  (             �                                                                                          '          
  �  9         
    
                    �             �                                                                                          9          
  P  N      �  
    
                  �  �  	           <                                                                                          N          
  �  d      x  
    
                  d  ,  
           �                                                                                          d          
  �  r      $                           �             �                                                                                          r            T        �                        �  �             @                                                                                                       	  �      |  
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
                                                                                          �            �  �      ,                          �             �                                                                                          �            \  �      �                        �  �             H                                                                                          �                �      �                        p               �                                                                                          �            p         �       t  X  �     �  t  6�      �         t             �          �      �              �       �  X          �  �      |         �         �              �      �                 P�                                               T�          `  �  L l@                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                  �  �  �  �  �                         �  �  �  �                             �                                       (  0  8                             <  H  P  \                             `  l  t  �                                                                          NroDoc  X(9)    Numero  Numero      RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ���������           �        �        �                �     i     i     i     	 	 	    q  �  �  �  �  
                                                                                                                                                           0  (                         4  <  H  P                             T  `  h  t                              x  �  �  �                             �  �  �  �                             �  �  �  �                              �  �  �                                                                            NroDoc  X(9)    Numero  Numero      RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  ���������           �        �        �                �     i     i     i     	 	 	    q  �  �  �  �  
      ��                            ����                            O    p�                    �&    undefined                                                               �       t�  �   l   ��  ��                    �����               ��X	                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                     3   ����       $      H  ���                       8      
                       � ߱        �  �   "   D       �     >          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   �8X	                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  S  V  L              X�W	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  X  ^  �              ,zY	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  `  a  p              �U	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  c  f  p              ��U	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  h  j  �               ~Y	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  l  o  �	              �W	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  q  r  H              ܬW	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  t  u  T              ,�U	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  w  y  T              ��U	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  {  |  |              �'X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  ~    |              �6X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  �  �  |              `7X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              PX	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              �X	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              $X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              �X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              ��W	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              H�W	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              �Y	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              ��Y	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              TX	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              t�W	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              �X	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              ��W	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              x�e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              �ne	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              8�d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              �	e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'              (e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              �e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                  �  �  �)              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                  �  �  @+              �!e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                  �  �  �,              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                    
  �-              8�e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                      �/              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                      �0              $�d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                      �1              t�d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                      �2              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     L       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 S       CHARACTER,  canNavigate �3      �3      (4    ]       LOGICAL,    closeQuery  4      44      `4   
 i       LOGICAL,    columnProps @4      l4      �4    t       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    �       CHARACTER,  hasForeignKeyChanged    88      d8      �8    �       LOGICAL,    openDataQuery   |8      �8      �8    
      LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	       LOGICAL,    prepareQuery    9      49      d9    "      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    /      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 <      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 F      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 P      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    Z      CHARACTER,  assignDBRow                             <  �;      ��                  �    <              �e	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                      L=              �e	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                  
    �>              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                      �?               �e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                       A              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                      PB              �`e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                      PC              @ce	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                      PD              �ze	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                      PE              �{e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                  !  "  PF              T�e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  $  &  \G              \�e	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  (  )  �H              �f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  +  -  �I              �f	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  /  0  �J              �f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  2  3  �K              (f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  5  8  �L              �f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    {      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP    �      CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P    �      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q           CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !        CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  !      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  0      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  ?      CHARACTER,  getForeignValues    @R      lR      �R  %  N      CHARACTER,  getQueryPosition    �R      �R      �R  &  _      CHARACTER,  getQuerySort    �R      �R      S  '  p      CHARACTER,  getQueryString  �R      (S      XS  (  }      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /  �      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0  �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1        CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2        LOGICAL,    removeQuerySelection    �W      �W      (X  3  -      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  B      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 P      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  [      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  j      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  {      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              LZf	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              �\f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]              X]f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              tDf	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              $Ef	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              �Hf	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b              0Pf	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              |hf	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �  �  Hd              �if	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  �      CHARACTER,  getASBound  �d      e      0e  = 
 �      LOGICAL,    getAsDivision   e      <e      le  >  �      CHARACTER,  getASHandle Le      xe      �e  ?  �      HANDLE, getASHasStarted �e      �e      �e  @  �      LOGICAL,    getASInfo   �e      �e      f  A 	 �      CHARACTER,  getASInitializeOnRun    �e       f      Xf  B        LOGICAL,    getASUsePrompt  8f      df      �f  C        LOGICAL,    getServerFileName   tf      �f      �f  D  (      CHARACTER,  getServerOperatingMode  �f      �f      g  E  :      CHARACTER,  runServerProcedure  �f      $g      Xg  F  Q      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  d      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  r      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              ,�f	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              �f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              h�f	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              �f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              l�f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              �f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              |�f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              l�f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              X�f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              �f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              (�f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              �f	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �  �  �{              l�f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  �  �  �|              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                  �  �  x~              p�f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                  �  �  �              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                  �    ��              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                      L�              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                      t�              �g	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 1      LOGICAL,    assignLinkProperty  ؃      �      8�  P  <      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  O      CHARACTER,  getChildDataKey ��      ̄      ��  R  ]      CHARACTER,  getContainerHandle  ܄      �      <�  S  m      HANDLE, getContainerHidden  �      D�      x�  T  �      LOGICAL,    getContainerSource  X�      ��      ��  U  �      HANDLE, getContainerSourceEvents    ��      ��      ��  V  �      CHARACTER,  getContainerType    ܅      �      <�  W  �      CHARACTER,  getDataLinksEnabled �      H�      |�  X  �      LOGICAL,    getDataSource   \�      ��      ��  Y  �      HANDLE, getDataSourceEvents ��      ��      �  Z  �      CHARACTER,  getDataSourceNames  Ԇ       �      4�  [        CHARACTER,  getDataTarget   �      @�      p�  \        CHARACTER,  getDataTargetEvents P�      |�      ��  ]  '      CHARACTER,  getDBAware  ��      ��      �  ^ 
 ;      LOGICAL,    getDesignDataObject ȇ      �      (�  _  F      CHARACTER,  getDynamicObject    �      4�      h�  `  Z      LOGICAL,    getInstanceProperties   H�      t�      ��  a  k      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  �      CHARACTER,  getObjectHidden �      <�      l�  d  �      LOGICAL,    getObjectInitialized    L�      x�      ��  e  �      LOGICAL,    getObjectName   ��      ��      �  f  �      CHARACTER,  getObjectPage   ̉      ��      (�  g  �      INTEGER,    getObjectParent �      4�      d�  h  �      HANDLE, getObjectVersion    D�      l�      ��  i  �      CHARACTER,  getObjectVersionNumber  ��      ��      �  j  
      CHARACTER,  getParentDataKey    Ċ      ��      $�  k  !      CHARACTER,  getPassThroughLinks �      0�      d�  l  2      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  F      CHARACTER,  getPhysicalVersion  ��      ��      �  n  \      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  o      CHARACTER,  getQueryObject  �      4�      d�  p  �      LOGICAL,    getRunAttribute D�      p�      ��  q  �      CHARACTER,  getSupportedLinks   ��      ��      ��  r  �      CHARACTER,  getTranslatableProperties   ��      �      (�  s  �      CHARACTER,  getUIBMode  �      4�      `�  t 
 �      CHARACTER,  getUserProperty @�      l�      ��  u  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  !	      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  /	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  <	      CHARACTER,  setChildDataKey 4�      `�      ��  }  K	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  [	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    n	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  �	      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  $
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  5
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  K
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  `
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  r
      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  �
      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �  �
      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �  �
      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �        LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 +      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  6      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  F      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 R      CHARACTER,INPUT pcName CHARACTER    l�      ��  0�      �       4   �����                 @�                      ��                    L                  2f	                         Ě           \�  ؛      �       4   �����                 �                      ��                  !  K                  �2f	                       !  l�  �    8  �  ��      �       4   �����                 ��                      ��                  D  F                  *f	                       D  �         E                                  ,     
                    � ߱        �  $  H  ��  ���                           $  J  @�  ���                       x                         � ߱        x�    P  ��  �      �      4   �����                �                      ��                  Q  	                  �*f	                       Q  ��  H�  o   T      ,                                 ��  $   U  t�  ���                       �  @         �              � ߱        ��  �   V        Ȟ  �   W  �      ܞ  �   Y        �  �   [  x      �  �   ]  �      �  �   _  `      ,�  �   `  �      @�  �   a        T�  �   d  �      h�  �   f         |�  �   g  |      ��  �   i  �      ��  �   j  t      ��  �   k  �      ̟  �   l  ,      ��  �   m  �      ��  �   s  �      �  �   u  P	      �  �   {  �	      0�  �   }   
      D�  �     t
      X�  �   �  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  <	  j	  ��              ̜g	                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ P	  آ  ���                           O   h	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  Z                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  
                  ��g	                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    A
  T�  Ц      x      4   ����x                �                      ��                  B
  �
                  �vf	                       B
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
  ~                  L�g	                       �
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
  H;      <�  �     �;      P�  �     8<          �     �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  -                  �e	                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
     �  O                  �g	                       �  <�  ̱  �   �  �K      $�  $  �  ��  ���                       �K     
                    � ߱        8�  �   �  L      ��  $   �  d�  ���                       ,L  @         L              � ߱        L�  $  �  ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V     �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $    x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   0  �  ���                                      ̵                      ��                  Q  �                  �g	                       Q  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   f  �  ���                        adm-clone-props �  ��              �     W     `                          \  T                     start-super-proc    �  d�  �           �     X                                  u                     l�      �   �      �X      4   �����X      /     ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $  !  ��  ���                       @Y                         � ߱        ��    1  �  \�  ��  \Y      4   ����\Y                и                      ��                  2  6                  �g	                       2  �  pY                     �Y                     �Y                         � ߱            $  3  l�  ���                             7  �  T�      �Y      4   �����Y  �Y                         � ߱            $  8  (�  ���                       �Y                         � ߱        ع  $  <  ��  ���                       Ժ    ?  ��  �  \�  �Y      4   �����Y      $  @  0�  ���                       Z                         � ߱            �   ]  ,Z      lZ     
                �Z                     8\  @        
 �[              � ߱         �  V   q  p�  ���                        �  �   �  D\      �    #  0�  @�      �\      4   �����\      /   $  l�     |�                          3   �����\            ��                      3   �����\  �\     
                P]                     �^  @        
 `^              � ߱        ��  V   0  ��  ���                        �^     
                h_                     �`  @        
 x`              � ߱        ̼  V   T  <�  ���                        L�    �  �  d�      �`      4   �����`                t�                      ��                  �  �                  ԩg	                       �  ��  �  /   �  ��     ��                          3   �����`            н                      3   �����`      /   �  �     �                          3   ����a            <�                      3   ����8a  ��  /  A  x�         la                      3   ����Ta  initProps   x�  ��              ,     Y     $                             x  	                                   ̿          t�  \�      ��                   2  ��              Ih	                    O   ����    e�          O   ����    R�          O   ����    ��      �                      ��          ��  p   $  �t  �      /  t�  d�     �t                                        ��                  %  A                  �Uh	                       %  ��   �  ��     �t                                        ��                  B  ^                  �ch	                       B  ��  ��  |�     �t                                        ��                  _  {                  xdh	                       _  �  �  �     u                                        ��                  |  �                  Heh	                       |  ��  ��  ��      u                                        ��                  �  �                  DZh	                       �  (�  0�   �     4u                                        ��                  �  �                  [h	                       �  ��  ��  ��     Hu                                        ��                  �  �                  �[h	                       �  @�  H�  8�     \u                                        ��                  �                    �\h	                       �  ��  ��  ��     pu  	                                      ��             	       )                  |]h	                         X�  `�  P�     �u  
                                      ��             
     *  F                  �jh	                       *  ��  ��  ��     �u                                        ��                  G  c                  pkh	                       G  p�  x�  h�     �u                                        ��                  d  �                  @lh	                       d  ��  �  ��     �u                                        ��                  �  �                  mh	                       �  ��  ��  ��     �u                                        ��                  �  �                  \fh	                       �  �  �  �     �u                                        ��                  �  �                  �fh	                       �  ��  ��  ��     �u                                        ��                  �  �                  �gh	                       �  ,�  4�  $�     v                                        ��                  �                    xhh	                       �  ��      ��     $v                                        ��                    .                  Hih	                         D�      O   1  ��  ��  8v               D�          ,�  8�   , �                                                       �     ��                            ����                            ��  d�  ��  �      ��     Z     L�                      � H�  �                     ��    G  �  ��      Dv      4   ����Dv                ��                      ��                  H  \                  �^h	                       H  �  ��  /   I  ��     ��                          3   ����Tv            ��                      3   ����tv  h�  /   J  (�     8�                          3   �����v            X�                      3   �����v  ��  /   O  ��     ��                          3   �����v            ��                      3   �����v      /   U   �     �                          3   ����w            0�                      3   ����(w  Hw     
                �w                     y  @        
 �x              � ߱        ��  V   �  @�  ���                        ��  $  �  ��  ���                       (y                         � ߱        Hy     
                �y                     {  @        
 �z              � ߱        ��  V   �  (�  ���                        t�  $  �  ��  ���                        {     
                    � ߱        4{     
                �{                      }  @        
 �|              � ߱        ��  V   �  �  ���                        \�  $  
  ��  ���                       }     
                    � ߱         }     
                �}                     �~  @        
 �~              � ߱        ��  V     ��  ���                        D�  $  .  ��  ���                                                � ߱        ,     
                �                     ��  @        
 ��              � ߱        p�  V   8  ��  ���                        ��  �   R  �      @�  $  S  ��  ���                       0�     
                    � ߱        D�     
                ��                     �  @        
 Ђ              � ߱        l�  V   ]  ��  ���                        ��  $  w  ��  ���                       �     
                    � ߱        ��  �   �  0�      0�  $  �  �  ���                       p�     
                    � ߱        D�  �   �  ��      ��  $  �  p�  ���                       ă                         � ߱              �  ��  ��      ��      4   ������      /   �  ��     �                          3   ���� �  4�     
   $�                      3   ���� �  d�        T�                      3   ����(�  ��        ��                      3   ����<�            ��                      3   ����X�  pushRowObjUpdTable  ��  ��  �                   [      �                               �                     pushTableAndValidate    ��  4�  �           |     \     �                          �  �                     remoteCommit    L�  ��  �           p     ]     �                          �                       serverCommit    ��  �  �           l     ^     �                          �  $                                     4�          �  ��      ��                      �              x�g	                    O   ����    e�          O   ����    R�          O   ����    ��          O     ��  ��  ��    ��                            ����                            $�  P�      ��              _      L�                      
�     1                     disable_UI  ��  ��                      `      �                               D  
                    �  �    ����  �       ��       ��  8   ����   ��  8   ����       8   ����       8   ����       ��  ��      viewObject  ,   ��  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  �  4�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  �  x�  ��      returnFocus ,INPUT hTarget HANDLE   h�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  ��  �      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  \�  l�      removeAllLinks  ,   L�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE p�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  t�  ��      hideObject  ,   d�  ��  ��      exitObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  ��  ��      displayLinks    ,   ��  �  �      createControls  ,   ��  (�  8�      changeCursor    ,INPUT pcCursor CHARACTER   �  d�  p�      applyEntry  ,INPUT pcField CHARACTER    T�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  �  �      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  h�  p�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE X�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  �      runServerObject ,INPUT phAppService HANDLE  ��  8�  L�      disconnectObject    ,   (�  `�  p�      destroyObject   ,   P�  ��  ��      bindServer  ,   t�  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  ��  �      startFilter ,   ��  �  (�      releaseDBRow    ,   �  <�  L�      refetchDBRow    ,INPUT phRowObjUpd HANDLE   ,�  x�  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE h�  ��  ��      fetchDBRowForUpdate ,   ��  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  (�  8�      compareDBRow    ,   �  L�  `�      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   <�  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  �  �      updateState ,INPUT pcState CHARACTER    ��  D�  X�      updateQueryPosition ,   4�  l�  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    \�  ��  ��      undoTransaction ,   ��  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  x�  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   h�  ��  �      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  P�  d�      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  @�  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  ,�  @�      startServerObject   ,   �  T�  d�      setPropertyList ,INPUT pcProperties CHARACTER   D�  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ��   �      rowObjectState  ,INPUT pcState CHARACTER    ��  ,�  <�      retrieveFilter  ,   �  P�  d�      restartServerObject ,   @�  x�  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   h�  ��  ��      refreshRow  ,   p�  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  �  $�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  �  T�  l�      initializeServerObject  ,   D�  ��  ��      initializeObject    ,   p�  ��  ��      home    ,   ��  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��   �  �      fetchPrev   ,   ��   �  ,�      fetchNext   ,   �  @�  L�      fetchLast   ,   0�  `�  l�      fetchFirst  ,   P�  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   p�  ��  ��      endClientDataRequest    ,   ��  ��  ��      destroyServerObject ,   ��  �  �      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    ��  h�  x�      dataAvailable   ,INPUT pcRelative CHARACTER X�  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  ��  �      commitTransaction   ,   ��   �  0�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    �  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 X	%     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        ��    ;   %               � 
"    
 �%              h �P  \         (          
�                          
�            � \   R	
"    
 g	
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 l	�               1� l  
 l	� w   �%               o%   o           � |    l	
"   
 l	�           �    1� }   l	� w   �%               o%   o           � �   l	
"   
 l	�           �    1� �  
 l	� w   �%               o%   o           � �   l	
"   
 l	�           l    1� �   l	� w   �%               o%   o           � |    l	
"   
 l	�           �    1� �   l	� w   �%               o%   o           � �   l	
"   
 l	�           T    1� �   l	� �   �%               o%   o           %               
"   
 ��          �    1� �   ��      
"   
 l	�               1� 
   l	� w   �%               o%   o           �   l	
"   
 l	�           �    1�    l	� w   �%               o%   o           � .  S l	
"   
 l	�           �    1� �   l	� �   �%               o%   o           %               
"   
 l	�           p    1� �   l	� �   �%               o%   o           %               
"   
 l	�           �    1� �   l	� �   �%               o%   o           %              
"   
 ��          h    1� �   �� �     
"   
 l	�           �    1� �  
 l	� �   �%               o%   o           %               
"   
 l	�                1� �   l	� w   �%               o%   o           � |    l	
"   
 ��          �    1� �   ��      
"   
 l	�           �    1� �   l	� w   �%               o%   o           � �  t l	
"   
 ��          D	    1� n  
 ��      
"   
 l	�           �	    1� y   l	� w   �%               o%   o           � �  � l	
"   
 l	�           �	    1�    l	� w   �%               o%   o           � |    l	
"   
 l	�           h
    1� .  
 l	� 9   �%               o%   o           %               
"   
 g	�           �
    1� =   g	� �   �%               o%   o           %              
"   
 e	�           `    1� E   e	� w   �%               o%   o           � |    g	
"   
 e	�           �    1� V   e	� w   �%               o%   o           o%   o           
"   
 f	�           P    1� f  
 f	� w   �%               o%   o           � |    g	
"   
 e	�           �    1� q   e	� �  	 �%               o%   o           � �  / f	
"   
 ��          8    1� �   �� �  	   
"   
 g	�           t    1� �   g	� �  	 �o%   o           o%   o           � |    g	
"   
 ��          �    1� �   �� �  	   
"   
 g	�           $    1� �   g	� �  	 �o%   o           o%   o           � |    g	
"   
 ��          �    1�     �� �     
"   
 ��          �    1�    �� �  	   
"   
 ��              1�    �� �  	   
"   
 ��          L    1� (   �� �  	   
"   
 g	�           �    1� 6   g	� �   �o%   o           o%   o           %              
"   
 ��              1� G   �� �  	   
"   
 ��          @    1� U  
 �� `     
"   
 ��          |    1� h   �� �  	   
"   
 ��          �    1� w   �� �  	   
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
 e	�                1� �   e	� w   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 g	
"   
   
"   
 R	(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�            �� �     p�               �L
�    %              � 8          � $         � �          
�    �      
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 g	�           �    1�   
 g	� w   �%               o%   o           � |    g	
"   
 g	�           <    1� &  
 g	� w   �%               o%   o           o%   o           
"   
 g	�           �    1� 1   g	�    �%               o%   o           o%   o           
"   
 e	�           4    1� :   e	� �   �%               o%   o           %               
"   
 g	�           �    1� I   g	� �   �%               o%   o           %               
"   
 X	�           ,    1� V   X	� w   �%               o%   o           � |    g	
"   
 g	�           �    1� ]   g	� �   �%               o%   o           %              
"   
 g	�               1� o   g	� �   �%               o%   o           o%   o           
"   
 f	�           �    1� {   f	� w   �%               o%   o           o%   o           
"   
 g	�               1� �  	 g	� w   �%               o%   o           � |    g	
"   
 g	�           �    1� �   g	� w   �%               o%   o           o%   o           
"   
 g	�               1� �   g	� w   �%               o%   o           o%   o           
"   
 g	�           �    1� �   g	� �   �%               o%   o           %               
"   
 g	�           �    1� �   g	� �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 g	�           �    1� �  
 g	� �   �%               o%   o           %              
"   
 g	�           H    1� �   g	� w   �%               o%   o           o%   o           
"   
 g	�           �    1� �   g	� w   �%               o%   o           � |    f	
"   
 g	�           8    1� �   g	� w   �%               o%   o           o%   o           
"   
 ��          �    1�    ��      
"   
 g	�           �    1�    g	� w   �%               o%   o           � #  ! g	
"   
 e	�           d    1� E   e	� w   �%               o%   o           � |    g	
"   
 g	�           �    1� R   g	� w   �%               o%   o           � e   e	
"   
 ��          L    1� t   �� �     
"   
 ��          �    1� �   ��      
"   
 g	�           �    1� �   g	� w   �%               o%   o           � |    g	
"   
 ��          8     1� �  
 ��      
"   
 X	�           t     1� �   X	� �   �%               o%   o           o%   o           
"   
 g	�           �     1� �   g	� �   �%               o%   o           %               
"   
 g	�           l!    1� �   g	� �   �%               o%   o           %               
"   
 f	�           �!    1� �   f	� w   �%               o%   o           � |    g	
"   
 f	�           \"    1� �   f	� w   �%               o%   o           o%   o           
"   
 g	�           �"    1� �   g	� �   �%               o%   o           %              
"   
 e	�           T#    1�    e	� �   �%               o%   o           %               
"   
 X	�           �#    1�    X	� �   �%               o%   o           %               
"   
 ��          L$    1� (   ��      
"   
 ��          �$    1� 5   �� w     
"   
 g	�           �$    1� B   g	� 9   �%               o%   o           o%   o           
"   
 f	�           @%    1� N   f	� w   �%               o%   o           � |    g	
"   
 f	�           �%    1� \   f	� w   �%               o%   o           o%   o           
"   
 g	�           0&    1� j   g	� �   �o%   o           o%   o           o%   o           
"   
 g	�           �&    1�    g	� �  	 �%               o%   o           o%   o           
"   
 g	�           ('    1� �   g	� w   �%               o%   o           o%   o           
"   
 g	�           �'    1� �  
 g	� 9   �%               o%   o           o%   o           
"   
 ��           (    1� �   �� w     
"   
 g	�           \(    1� �   g	� w   �%               o%   o           � �  4 g	
"   
 e	�           �(    1�   
 e	� �   �%               o%   o           %              
"   
 ��          L)    1�    ��      
"   
 g	�           �)    1� !   g	� w   �%               o%   o           � |    X	
"   
 g	�           �)    1� /   g	� �   �%               o%   o           %              
"   
 f	�           x*    1� >   f	� w   �%               o%   o           � |    g	
"   
 g	�           �*    1� K   g	� w   �%               o%   o           � |    f	
"   
 g	�           `+    1� Y   g	� w   �%               o%   o           � |    g	
"   
 g	�           �+    1� e   g	� �   �%               o%   o           %               
"   
 g	�           P,    1� t  	 g	�    �%               o%   o           o%   o           
"   
 X	�           �,    1� ~   X	� w   �%               o%   o           � �  	 e	
"   
 g	�           @-    1� �   g	� 9   �%               o%   o           %       �       
"   
 g	�           �-    1� �   g	� w   �%               o%   o           � |    g	
"   
 g	�           0.    1� �   g	� �   �o%   o           o%   o           %              
"   
 g	�           �.    1� �   g	� �   �%               o%   o           %               
"   
 g	�           (/    1� �   g	� w   �%               o%   o           o%   o           
"   
 g	�           �/    1� �   g	� �  	 �%               o%   o           � |    g	
"   
 ��          0    1� �   �� �  	   P �L 
�H T   %              �     }        �GG %              
"   
 X	�           �0    1�   
 X	� w   �%               o%   o           � |    X	
"   
 g	�           1    1�    g	� �   �%               o%   o           %               
"   
 g	�           �1    1�   	 g	� w   �%               o%   o           � |    g	
"   
 g	�           2    1� $   g	� w   �%               o%   o           � |    g	
"   
 g	�           �2    1� 2   g	� �   �%               o%   o           %               
"   
 g	�           �2    1� B   g	� w   �%               o%   o           � |    g	
"   
 g	�           p3    1� U   g	� w   �%               o%   o           o%   o           
"   
 g	�           �3    1� ]   g	� w   �%               o%   o           o%   o           
"   
 e	�           h4    1� j   e	� �   �%               o%   o           o%   o           
"   
 X	�           �4    1� x   X	� �   �%               o%   o           o%   o           
"   
 g	�           `5    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           �5    1� �   g	� w   �%               o%   o           o%   o           
"   
 g	�           X6    1� �  	 g	� �  	 �%               o%   o           � |    g	
"   
 f	�           �6    1� �  
 f	� �  	 �%               o%   o           � |    g	
"   
 g	�           @7    1� �   g	� w   �%               o%   o           � |    f	
"   
 g	�           �7    1� �   g	� w   �%               o%   o           o%   o           
"   
 g	�           08    1� �   g	� w   �%               o%   o           o%   o           
"   
 g	�           �8    1� �   g	� w   �%               o%   o           � |    g	
"   
 g	�            9    1� �   g	� w   �%               o%   o           � |    g	
"   
 g	�           �9    1�    g	� �  	 �%               o%   o           o%   o           
"   
 ��          :    1�    ��      
"   
 e	�           L:    1� )   e	� w   �%               o%   o           � |    g	
"   
 e	�           �:    1� 7   e	� w   �%               o%   o           o%   o           
"   
 X	�           <;    1� J   X	� �   �%               o%   o           o%   o           
"   
 g	�           �;    1� \  
 g	� w   �%               o%   o           � |    g	
"   
 g	�           ,<    1� g   g	� w   �%               o%   o           � |    g	
"   
 f	�           �<    1�    f	� �   �%               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 f	�           p=    1� �  	 f	�    �%               o%   o           o%   o           
"   
 g	�           �=    1� �   g	�    �%               o%   o           o%   o           
"   
 g	�           h>    1� �   g	�    �%               o%   o           o%   o           
"   
 X	�           �>    1� �   X	� �   �%               o%   o           %              
"   
 g	�           `?    1� �   g	� w   �%               o%   o           � �  M X	
"   
 g	�           �?    1� 3   g	� �   �%               o%   o           %              
"   
 g	�           P@    1� D   g	� �   �%               o%   o           %               
"   
 g	�           �@    1� X   g	� �   �%               o%   o           %               
"   
 g	�           HA    1� o   g	� �  	 �%               o%   o           � }   g	
"   
 g	�           �A    1� �   g	� �   �%               o%   o           %               
"   
 g	�           8B    1� �   g	� �  	 �%               o%   o           o%   o           
"   
 g	�           �B    1� �   g	� �   �o%   o           o%   o           %              
"   
 X	�           0C    1� �   X	� �  	 �o%   o           o%   o           � |    X	
"   
 g	�           �C    1� �   g	�    �o%   o           o%   o           o%   o           
"   
 g	�            D    1� �   g	�    �o%   o           o%   o           o%   o           
"   
 g	�           �D    1� �   g	� �  	 �o%   o           o%   o           o%   o           
"   
 g	�           E    1� �   g	�    �o%   o           o%   o           o%   o           
"   
 g	�           �E    1�    g	� �  	 �o%   o           o%   o           �    g	
"   
 g	�           F    1�    g	� �  	 �o%   o           o%   o           � -   g	
"   
 g	�           |F    1� 9   g	� �   �%               o%   o           %               
"   
 e	�           �F    1� M   e	� �   �%               o%   o           %               
"   
 ��          tG    1� a   �� �  	   
"   
 g	�           �G    1� u   g	� �   �%               o%   o           %               
"   
 g	�           ,H    1� �   g	� w   �%               o%   o           o%   o           
"   
 g	�           �H    1� �   g	� w   �%               o%   o           o%   o           
"   
 g	�           $I    1� �   g	� �   �%               o%   o           o%   o           
"   
 g	�           �I    1� �   g	� w   �%               o%   o           � |    g	
"   
 g	�           J    1� �   g	� �   �%               o%   o           %               
"   
 e	�           �J    1� �  	 e	� �   �%               o%   o           %                "    �%     start-super-proc v�%     adm2/smart.p �R	P �L 
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
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        �M    �� �   � P   �        �M    �@    
� @  , 
�       �M    �� �   R	p�               �L
�    %              � 8       N    � $         � �          
�    �    R	
"   
 �p� @  , 
�       O    �� 
   �p�               �L"  	  , �   �    g	�    ��     }        �A      |    "  	    �    g	%              (<   \ (    |    �     }        �A�    �A"  
  g	    "  	  R	"  
  g	  < "  	  R	"  
  g	(    |    �     }        �A�    �A"  
  g	
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   R	p�               �L
�    %              � 8      Q    � $         � �          
�    �    R	
"   
 �p� @  , 
�       R    �� l  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 h	
"   
   
"   
   (�  L ( l       �        �R    �� �   � P   �        �R    �@    
� @  , 
�       �R    �� �     p�               �L
�    %              � 8      �R    � $         � �          
�    �      
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
�       U    �� �    p�               �L(        � |      � |      � |      �     }        �A
�H T   %              �     }        �GG %              
"   
 g	 (   � 
"   
 R	    �        �U    �� �   �
"   
   � 8      DV    � $         � �          
�    �    R	
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
   p�    � C   g	
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 R	    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 g	"      �       }        �
"   
 �%              %                "    �%     start-super-proc u�%     adm2/appserver.p �g	�    � �     
�    �     }        �%               %      Server  - �     }        �    "    e	� |    �%               %      Client      "    e	� |    �%      NONE    p�,  8         $     "    g	        � �   R	
�    
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        �Z    �� �   � P   �        �Z    �@    
� @  , 
�       �Z    �� �   R	p�               �L
�    %              � 8      �Z    � $         � �          
�    �    R	
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "    g	        � �   R	
�     "    �%     start-super-proc t�%     adm2/dataquery.p �g	
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
 R	(�  L ( l       �         ]    �� �   � P   �        ,]    �@    
� @  , 
�       8]    �� �   R	p�               �L
�    %              � 8      D]    � $         � �   R	     
�    �    R	
"   
 �p� @  , 
�       T^    �� y   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
 R	(�  L ( l       �        8_    �� �   � P   �        D_    �@    
� @  , 
�       P_    �� �   R	p�               �L
�    %              � 8      \_    � $         � �   R	     
�    �    R	
"   
 �p� @  , 
�       l`    �� �   �p�               �L%               "    �%     start-super-proc t�%     adm2/query.p �R	%     start-super-proc t�%     adm2/queryext.p % 	    initProps R	
�    %8 , (   FOR EACH CcbCDocu NO-LOCK INDEXED-REPOSITION �R	�   � a     � c     � e     
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        (b    �� �   � P   �        4b    �@    
� @  , 
�       @b    �� �   R	p�               �L
�    %              � 8      Lb    � $         � �          
�    �    R	
"   
 �p� @  , 
�       \c    �� �   �p�               �L"    ,     "    g	� n    �
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �         d    �� �   � P   �        ,d    �@    
� @  , 
�       8d    �� �   R	p�               �L
�    %              � 8      Dd    � $         � �          
�    �    R	
"   
 �p� @  , 
�       Te    ��   	 �p�               �L"    , %               �    "      � c         %              %                   "      %                  "      "      "     T(        "    g	%              "    g	� c   �"      �       "    R	�    "    g	�    �� |      �    R	�    "     �     S    "      "    �    "    g	%                � @    �     t T     P   4       �"      (0       4       h	"      � |      � |    R	� a   h	T ,  %              T   "    h	"    �� c     �    R	� a   h	T    �    "    h	�    �"      �    R	"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    R	%              � |    �� o     4  h	     "      
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        j    �� �   � P   �        (j    �@    
� @  , 
�       4j    �� �   R	p�               �L
�    %              � 8      @j    � $         � �          
�    �    R	
"   
 �p� @  , 
�       Pk    ��   
 �p�               �L"    ,       "  
  g	�    � q   e	� c   �      "  	    �    � q   �� c   e	�   � a     � c     � q   R	�   � a     � c   R	� q   e	�   � a     � c     � q     
�H T   %              �     }        �GG %              
"   
 �
"   
 R	
"   
 �
"   
 �(�  L ( l       �        �l    �� �   � P   �        �l    �@    
� @  , 
�       �l    �� �   �p�               �L
�    %              � 8      m    � $         � �          
�    �      
"   
 �p� @  , 
�       n    �� �   �p�               �L"    , 
"   
   p� @  , 
�       ln    �� g     p�               �L"    , 
"   
  p� @  , 
�       �n    �� B    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � q     � c         "  	  h	�     "    g	T    "      "      @ A,    �   � a   �� o     "    R	"       T      @   "    �(        "      � |    R	� |      � a   R	"    h	     "  	   %              D H   @ A,    �   � a   R	� o     "    R	"    g	,    S   "    R	� q   g	� c   �%                T      @   "    �(        "      � |    R	� |      � a   R	"    g	     "  
   %                         "    �� o     "    R	           "      � o   R	"      
�H T   %              �     }        �GG %              
"   
 g	
"   
   
"   
 g	
"   
 R	(�  L ( l       �        �r    �� �   � P   �        �r    �@    
� @  , 
�       �r    �� �   g	p�               �L
�    %              � 8      s    � $         � �   R	     
�    �    �
"   
 �p� @  , 
�       t    �� g   �p�               �L"    , 
"   
   p� @  , 
�       lt    �� B     p�               �L"    , "      %              %              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    �%     start-super-proc s�%     adm2/data.p %     start-super-proc s�%     adm2/dataext.p 	%     start-super-proc s�%     adm2/dataextcols.p 	%     start-super-proc s�%     adm2/dataextapi.p h	
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
 R	(�  L ( l       �        �w    �� �   � P   �        �w    �@    
� @  , 
�       �w    �� �   R	p�               �L
�    %              � 8      �w    � $         � �   R	     
�    �    R	
"   
 �p� @  , 
�       �x    �� u   �p�               �L%               %     "Util/dccbcdocu.i"  
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        �y    �� �   � P   �        �y    �@    
� @  , 
�       �y    �� �   R	p�               �L
�    %              � 8      �y    � $         � �          
�    �    R	
"   
 �p� @  , 
�       �z    �� o   �p�               �L"    , 
�     	         �G
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        �{    �� �   � P   �        �{    �@    
� @  , 
�       �{    �� �   R	p�               �L
�    %              � 8      �{    � $         � �          
�    �    R	
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
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        l}    �� �   � P   �        x}    �@    
� @  , 
�       �}    �� �   R	p�               �L
�    %              � 8      �}    � $         � �          
�    �    R	
"   
 �p� @  , 
�       �~    �� t  	 �p�               �L
"   
 , 
"   
 �     � X  	   �        �~    �
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        x    �� �   � P   �        �    �@    
� @  , 
�       �    �� �   R	p�               �L
�    %              � 8      �    � $         � �          
�    �    R	
"   
 �p� @  , 
�       ��    �� �   �p�               �L"    , 
"   
   �       �    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 R	
"   
 �
"   
 R	
"   
   (�  L ( l       �        ��    �� �   � P   �        ��    �@    
� @  , 
�       ��    �� �   R	p�               �L
�    %              � 8      ��    � $         � �          
�    �    R	
"   
 �p� @  , 
�       Ă    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 X	        � b   R	
�    
�             �Gp�,  8         $     
"   
 X	        � t   R	
�    �    � �     
�        "    g	� |    �%     modifyListProperty 
�    %      REMOVE  %     SupportedLinks 	%     Update-Target  	%     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    �       
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   l       ��                 ^  �  �               X�g	                    O   ����    e�          O   ����    R�          O   ����    ��        $  m  �   ���                       �U     
                    � ߱              n  (  �      V      4   ����V                �                      ��                  o  �                  h	                       o  8  �  �  p  PV            r  �  `      �V      4   �����V                p                      ��                  s  �                  �h	                       s  �  �  o   t      ,                                 �  �   u  �V      �  �   v  �V      $  $  w  �  ���                        W     
                    � ߱        8  �   x  @W      L  �   y  `W      `  �   |  �W          $     �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               �h	                    O   ����    e�          O   ����    R�          O   ����    ��      d                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  �h	                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��                 M    �               �g	                    O   ����    e�          O   ����    R�          O   ����    ��        $  m  �   ���                       ta                         � ߱        �  $  n  8  ���                       �a                         � ߱        �a     
                Xb                     �c  @        
 hc              � ߱        �  V   �  d  ���                        �    �    �      �c      4   �����c  �c     
                Pd                     �e  @        
 `e              � ߱            V   �     ���                          $    �  ���                       �e                         � ߱        �  $    4  ���                       �e                         � ߱          �      4  8                      ��        0           1                  TMh	      Xf     �       `      $      ���                       �e                         � ߱        �  $    `  ���                       f                         � ߱            4   ����8f  df                     �f                     �f                     g                     (g                         � ߱        d  $    �  ���                             )  �  �      Hg      4   ����Hg      $  *  �  ���                       pg          �h             � ߱        �  $  4    ���                       �h                         � ߱          �        |                      ��        0         6  ;                  �Ph	      <i     8     6  @      $  6  �  ���                       �h                         � ߱        l  $  6  @  ���                       �h                         � ߱            4   ����i      $  8  �  ���                       Pi                         � ߱        �i     
                Lj                     �k  @        
 \k              � ߱        �  V   F  �  ���                        �k       
       
       �k       	       	       l                     <l                         � ߱        	  $  �  d  ���                       
  $  1  <	  ���                       hl                         � ߱        �l     
                m                     `n  @        
  n          �n  @        
 xn          o  @        
 �n              � ߱        �
  V   =  h	  ���                          �
        |                      ��        0         �  �                  �Fh	      �o     T     �  0
      $  �  �
  ���                       o                         � ߱        \  $  �  0  ���                       Lo                         � ߱        l  4   ����to      4   �����o  �  $  �  �  ���                       p                         � ߱        �    �  �  l      4p      4   ����4p                �                      ��                  �  �                  Gh	                       �     xp                     �p       	       	           � ߱            $  �  |  ���                             �    �      q      4   ����q                �                      ��                  �  �                  �Gh	                       �    �q                     r       
       
           � ߱            $  �  �  ���                       ,r                     `r                         � ߱           $  �    ���                       �r     
                s                     `t  @        
  t          �t  @        
 xt              � ߱            V   �  �  ���                                    7 �          �  d  � `                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             �   �   �   �   �   �   �   �       $  4  D  T  d  t  �  \  �  �  �  �  8HXhx��������(8HX      �   �   �   �   �   �   �   �      $  4  D  T  d  t  �   \ �  �  �  �  8HXhx��������(8HX  �                    � �                     �          ��                            ����                                                        �   l       ��                  u  �  �               �3g	                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                  �  �  �               �6g	                    O   ����    e�          O   ����    R�          O   ����    ��      �       �              �                  $                  d  /  �  $     4  ��                      3   ����t�            T                      3   ������      O   �  ��  ��  ��               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  �  �  �               D�g	                    O   ����    e�          O   ����    R�          O   ����    ��      �       �              �                $                  �       ,             �          �                                �  /  �  t     �  ̄                      3   ������            �                      3   ����Ԅ     /  �  �     �  ��                      3   ������  x                             3   �����      $   �  L  ���                                                   � ߱                  �  �                  3   �����      $   �  �  ���                                                   � ߱        X  $  �  ,  ���                       �                         � ߱            O   �  ��  ��  8�               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                  �  �  �               �g	                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  �                    �          �                      �              /  �  L     \  h�                      3   ����L�  �        |  �                  3   ����p�      $   �  �  ���                                                   � ߱                                      3   ����|�      $   �  @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                  J  U  �               ��g	                    O   ����    e�          O   ����    R�          O   ����    ��            T  �   �       ��      4   ������      �   T  ��    ��                            ����                            TXS appSrvUtils O:\on_in_co\Util\dccbcdocu.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "Util/dccbcdocu.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server Client NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH CcbCDocu NO-LOCK INDEXED-REPOSITION ,   CcbCDocu  ; NroDoc INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery x  �,  �  �:      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable             |     cQueryString    |  �  "   Y   �          �                  initProps   m  n  �  �  �          )  *  1  4  6  8  ;  F  �  1  =  �  �  �  �  �  �  �  �  �  �  �  �  �              d     lRet              �        piTableIndex    �  �  (   Z   P  l      �                  deleteRecordStatic  $  %  A  B  ^  _  {  |  �  �  �  �  �  �  �  �      )  *  F  G  c  d  �  �  �  �  �  �  �  �  �  �      .  /  1  2                 !       �  �     [       x      �                  pushRowObjUpdTable  �  �        �        pcValType                  $       �  \     \       �      D                  pushTableAndValidate    �  �  �  �        �        pcContext   �             $       �        �        pcMessages            �        pcUndoIds     ,     ]       h                        remoteCommit    �  �  �  �  �  X             $       |        p        pcMessages            �        pcUndoIds   �  �     ^       @      �                  serverCommit    �  �  �  ,     _                                 getRowObjUpdStatic      �  p     `               d                  disable_UI  T  U  4  D                                   H  �  �     RowObject                              (         0         <         NroDoc  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     X  d     RowObjUpd   �         �         �         �         �         �         �         NroDoc  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   $            
   appSrvUtils L       8     xiRocketIndexLimit  t        `  
   gshAstraAppserver   �        �  
   gshSessionManager   �        �  
   gshRIManager    �        �  
   gshSecurityManager          �  
   gshProfileManager   <        $  
   gshRepositoryManager    h  	 	     P  
   gshTranslationManager   �  
 
     |  
   gshWebManager   �        �     gscSessionId    �        �     gsdSessionObj   �        �  
   gshFinManager             
   gshGenManager   @        0  
   gshAgnManager   d        T     gsdTempUniqueID �        x     gsdUserObj  �        �     gsdRenderTypeObj    �        �     gsdSessionScopeObj  �       �  
   ghProp           
   ghADMProps  4       $  
   ghADMPropsBuf   \       H     glADMLoadFromRepos  x       p     glADMOk �       �  
   ghContainer �    	   �     cObjectName �    
   �     iStart  �       �     cAppService             cASDivision @       (     cServerOperatingMode    d       T     cContainerType  �       x     cQueryString    �       �  
   hRowObject  �       �  
   hDataQuery  �       �     cColumns             �     cDataFieldDefs  (    X    RowObject         X  8  RowObjUpd          "   >   �   �   �   �          !  8  D  E  F  H  J  K  L  P  Q  T  U  V  W  Y  [  ]  _  `  a  d  f  g  i  j  k  l  m  s  u  {  }    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
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
        ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  -  �  �  �  �  �  �  �  �  �      0  O  Q  f  �      !  1  2  3  6  7  8  <  ?  @  ]  q  �  #  $  0  T  �  �  �  �  �  A  G  H  I  J  O  U  \  �  �  �  �  �  
    .  8  R  S  ]  w  �  �  �  �  �  �      ,�  O:\on_in_co\Util\dccbcdocu.w p"  ��  C:\Progress\OpenEdge\src\adm2\data.i �"  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    �"  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i #  % , O:\on_in_co\Util\dccbcdocu.i <#  �:   C:\Progress\OpenEdge\src\adm2\query.i    d#  z + C:\Progress\OpenEdge\src\adm2\delrecst.i �#  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  �#   ) %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i    $  � ! C:\Progress\OpenEdge\src\adm2\dataquery.i    @$  �Z ( %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   x$  �< " C:\Progress\OpenEdge\src\adm2\appserver.i    �$  �� ' %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �$  I� # C:\Progress\OpenEdge\src\adm2\smart.i    8%  Ds & C:\Progress\OpenEdge\gui\fn  l%  tw % %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �%  Q. $ C:\Progress\OpenEdge\gui\set �%  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i �%  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    0&  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    t&  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  �&  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i �&  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i ,'   
 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    l'  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   �'  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   �'  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i 8(  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    l(  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �(  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �(  �j  C:\Progress\OpenEdge\gui\get ()  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    P)  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �)  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �)  Su  C:\Progress\OpenEdge\src\adm2\globals.i  *  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i @*  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �*  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �*  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �*  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    @+  ª  %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   |+  �� 	 C:\Progress\OpenEdge\src\adm2\qryprto.i  �+  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   �+  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i <,  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    p,  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �,  ��    O:\on_in_co\Util\dccbcdocu_cl.w         [      -  �        (-  [  �     8-     �  &   H-  �   A     X-     �  .   h-  �   �     x-     �     �-  �   �     �-     �  $   �-  �   �     �-     y  $   �-  �   w     �-     U  $   �-  �   R     �-     0  $   .  �   .     .       $   (.  �   	     8.     �  $   H.  �   �     X.     �  $   h.  �   �     x.     �  $   �.  �   �     �.     z  -   �.  �   v     �.     t  ,   �.  k   :     �.  �  .      �.       +   �.  �        /     �  +   /  �  �      (/     �  +   8/  �  �      H/     �  +   X/  �  �      h/     �  +   x/  �  �      �/     �  +   �/  �  �      �/     f  +   �/  �  c      �/     I  +   �/  �  F      �/     ,  +   �/  �  )      0       +   0  �        (0     �  +   80  �  �      H0     �  +   X0  �  �      h0     �  +   x0  �  �      �0     �  +   �0  �  �      �0     ~  +   �0  �  {      �0     a  +   �0  �  ^      �0     D  +   �0  �  A      1     '  +   1  �        (1     �  $   81  �  �      H1     �  $   X1  k  �      h1     {  $   x1  j  z      �1     X  $   �1  i  W      �1     5  $   �1  _  +      �1       *   �1  ^        �1     �  *   �1  ]  �      2     �  *   2  \  �      (2     �  *   82  [  �      H2     i  *   X2  Z  h      h2     B  *   x2  Y  A      �2       *   �2  X        �2     �  *   �2  W  �      �2     �  *   �2  V  �      �2     �  *   �2  U  �      3       *   3  T  ~      (3     X  *   83  S  W      H3     1  *   X3  R  0      h3     
  *   x3  Q  	      �3     �  *   �3  P  �      �3     �  *   �3  O  �      �3     �  *   �3  N  �      �3     n  *   �3  @  `      4     >  $   4          (4     �  $   84    �      H4     �  $   X4  �   =      h4     �  )   x4  g   �      �4  a   �  !   �4     p  (   �4  _   n  !   �4     L  $   �4  ]   J  !   �4     (  $   �4  I     !   �4  �     "   5     �  '   5  �   �  "   (5     �  $   85  �   �  "   H5     i  $   X5  �   g  "   h5     E  $   x5  g   +  "   �5          �5  O   �  "   �5  �   ~  #   �5     |  &   �5  �   L  #   �5     �  %   �5  �   �  #   �5     �  $   6  �   �  #   6     �  $   (6  �   �  #   86     �  $   H6  �   �  #   X6     ^  $   h6  �   J  #   x6     (  $   �6  }     #   �6     �  $   �6     ~  #   �6     0  "   �6     �  !   �6     �      �6     6     �6  �   -     7  O        7          (7     �     87  �   �     H7  �   ~     X7  O   p     h7     _     x7          �7  y   �
     �7  �   �
  
   �7  G   �
     �7     �
     �7     y
     �7  c   
  
   �7  x   
     �7  M   �	     8     �	     8     �	     (8  a   �	     88  �  g	     H8     H	     X8  �  	     h8  O   	     x8     �     �8     �     �8  �   �     �8     �     �8     �     �8  x   �     �8     �     �8     c     �8     _     9     K     9     2     (9  Q   "     89     �     H9     �     X9     |     h9     b     x9  ]   \  
   �9     R     �9     
  
   �9     �     �9     �  
   �9  Z   �     �9     �  	   �9     �     �9     �     :     �     :  c   f     (:     D     8:     �      H:     �      X:     �      h:     �      x:     &      �:           �:           