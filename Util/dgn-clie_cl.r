	��V�8�K�6   �                                               36FC00EFutf-8 MAIN O:\on_in_co\Util\dgn-clie_cl.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,NomCli character 0 0,DirCli character 1 0,NroCard character 2 0,Ruc character 3 0,CodDept character 4 0,RowNum integer 5 0,RowIdent character 6 0,RowMod character 7 0,RowIdentIdx character 8 0,RowUserProp character 9 0,ChangedFields character 10 0      D              T             �� D  �              ��              P;     +   � �  W   �� `  X   � d  Y   l�   [   |�   \   �� <  ]   Я    ^   � 0  `   ?  � �  iSO8859-1                                                                           �    �                                      �                   ��                �  �       ��   T�              ��  �                                                                  PROGRESS                         �           
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
                                                                                          �            �  �      ,                          �             �                                                                                          �            \  �      �                        �  �             H                                                                                          �                �      �                        p               �                                                                                          �            p         �       s  X  x     �  s  @G      �  
       s             �          �      �              �       �  X          �        |         �         �                    �                 P�                                               T�          `  �  L l@                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                   �  �  �  �  �          �             �  �                              4  <  D  T  L                         X  \  d  l  h          p             �  �  �  �  �                         �  �  �  �                             �  �                                      $  ,                             0  <  D  P                             T  `  h  t                                                                          NomCli  x(50)   Nombre  Nombre      Nombre del Cliente  DirCli  x(60)   Direcci�n   Direcci�n       Direcci�n del Cliente   NroCard x(8)    NroCard Nrocard     Ruc x(11)   Ruc Ruc     Registro Unico de Contribuyente (Cliente)   CodDept X(3)    Departamento    Departamento        RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ���������                        0        7                �     i     i     i     		 	
 	    �             '  0  7  C                                                                                                                                     	                  
                                                                          (  8  0          <             P  X  `  x  l          |             �  �  �  �  �                         �  �  �  �  �          �             �      ,                           0  8  D  L                             P  \  d  p                              t  |  �  �                             �  �  �  �                             �  �  �  �                              �  �  �                                                                             NomCli  x(50)   Nombre  Nombre      Nombre del Cliente  DirCli  x(60)   Direcci�n   Direcci�n       Direcci�n del Cliente   NroCard x(8)    NroCard Nrocard     Ruc x(11)   Ruc Ruc     Registro Unico de Contribuyente (Cliente)   CodDept X(3)    Departamento    Departamento        RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  ���������                        0        7                �     i     i     i     		 	
 	    �             '  0  7  C  O    ��                            ����                            �    p�                    c�    undefined                                                               �       t�  �   l   ��  ��                    �����               ��f	                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                     3   ����       $      H  ���                       8      
                       � ߱        �  �   "   D       �     >          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   LVf	                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  S  V  L              d�f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  X  ^  �              �e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  `  a  p              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  c  f  p              `�d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  h  j  �              �_e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  l  o  �	              �zf	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  q  r  H              Ĵf	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  t  u  T              t�f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  w  y  T              (�f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  {  |  |              hje	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  ~    |              �je	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  �  �  |              xke	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              `�f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              �se	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              h�e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              �f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              �e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              (e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              �e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              �e	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              `ce	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              �e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              �#f	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              @<f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              �Pf	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'              0�d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                  �  �  �)               �e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                  �  �  @+               f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                  �  �  �,              f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                    
  �-              �(f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                      �/              ��d	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                      �0              ,�d	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                      �1              �f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                      �2              tle	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     K       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 R       CHARACTER,  canNavigate �3      �3      (4    \       LOGICAL,    closeQuery  4      44      `4   
 h       LOGICAL,    columnProps @4      l4      �4    s       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	        CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    �       CHARACTER,  hasForeignKeyChanged    88      d8      �8    �       LOGICAL,    openDataQuery   |8      �8      �8    	      LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	       LOGICAL,    prepareQuery    9      49      d9    !      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    .      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 ;      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 E      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 O      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    Y      CHARACTER,  assignDBRow                             <  �;      ��                  �    <              ��e	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                      L=              �
e	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                  
    �>              f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                      �?              �f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                       A              X�e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                      PB              �Bf	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                      PC              hEf	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                      PD              Lf	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                      PE              ppf	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                  !  "  PF              0qf	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  $  &  \G              xtf	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  (  )  �H              �`f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  +  -  �I              `hf	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  /  0  �J              PHf	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  2  3  �K              �Hf	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  5  8  �L              p�f	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    z      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP    �      CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P    �      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q           CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !        CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "         HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  /      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  >      CHARACTER,  getForeignValues    @R      lR      �R  %  M      CHARACTER,  getQueryPosition    �R      �R      �R  &  ^      CHARACTER,  getQuerySort    �R      �R      S  '  o      CHARACTER,  getQueryString  �R      (S      XS  (  |      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /  �      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0  �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1        CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2        LOGICAL,    removeQuerySelection    �W      �W      (X  3  ,      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  A      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 O      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  Z      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  i      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  z      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              �g	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              tg	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]              @Zg	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              [g	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              �g	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              �g	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b              ,f	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              �Bg	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �  �  Hd              �e	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  �      CHARACTER,  getASBound  �d      e      0e  = 
 �      LOGICAL,    getAsDivision   e      <e      le  >  �      CHARACTER,  getASHandle Le      xe      �e  ?  �      HANDLE, getASHasStarted �e      �e      �e  @  �      LOGICAL,    getASInfo   �e      �e      f  A 	 �      CHARACTER,  getASInitializeOnRun    �e       f      Xf  B        LOGICAL,    getASUsePrompt  8f      df      �f  C        LOGICAL,    getServerFileName   tf      �f      �f  D  '      CHARACTER,  getServerOperatingMode  �f      �f      g  E  9      CHARACTER,  runServerProcedure  �f      $g      Xg  F  P      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  c      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  q      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I        LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              0h	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              D+h	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              �2h	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              h:h	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              4Oh	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              0Ch	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              Fh	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              $Ih	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              8Xh	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              �Xh	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              |Yh	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              \Th	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              lUh	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z              `ih	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �  �  �{              �g	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  �  �  �|              ��g	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                  �  �  x~              �eh	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                  �  �  �              ��g	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                  �    ��              �g	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                      L�              �.h	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                      t�              ��f	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
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
 *      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  5      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  E      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 Q      CHARACTER,INPUT pcName CHARACTER    l�      ��  0�      �       4   �����                 @�                      ��                    L                  L�f	                         Ě           \�  ؛      �       4   �����                 �                      ��                  !  K                  ��f	                       !  l�  �    8  �  ��      �       4   �����                 ��                      ��                  D  F                  0g	                       D  �         E                                  ,     
                    � ߱        �  $  H  ��  ���                           $  J  @�  ���                       x                         � ߱        x�    P  ��  �      �      4   �����                �                      ��                  Q  	                  �0g	                       Q  ��  H�  o   T      ,                                 ��  $   U  t�  ���                       �  @         �              � ߱        ��  �   V        Ȟ  �   W  �      ܞ  �   Y        �  �   [  x      �  �   ]  �      �  �   _  `      ,�  �   `  �      @�  �   a        T�  �   d  �      h�  �   f         |�  �   g  |      ��  �   i  �      ��  �   j  t      ��  �   k  �      ̟  �   l  ,      ��  �   m  �      ��  �   s  �      �  �   u  P	      �  �   {  �	      0�  �   }   
      D�  �     t
      X�  �   �  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  <	  j	  ��              lDg	                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ P	  آ  ���                           O   h	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  Y                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  
                  #h	                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    A
  T�  Ц      x      4   ����x                �                      ��                  B
  �
                  LJf	                       B
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
  ~                  ,�g	                       �
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
  H;      <�  �     �;      P�  �     8<          �     �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  -                  l�e	                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
     �  O                  ��g	                       �  <�  ̱  �   �  �K      $�  $  �  ��  ���                       �K     
                    � ߱        8�  �   �  L      ��  $   �  d�  ���                       ,L  @         L              � ߱        L�  $  �  ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V     �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $    x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   0  �  ���                                      ̵                      ��                  Q  �                  ��g	                       Q  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   f  �  ���                        adm-clone-props �  ��              �     W     `                          \  R                     start-super-proc    �  d�  �           �     X                                  s                     l�      �   �      �X      4   �����X      /     ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $  !  ��  ���                       @Y                         � ߱        ��    1  �  \�  ��  \Y      4   ����\Y                и                      ��                  2  6                  �g	                       2  �  pY                     �Y                     �Y                         � ߱            $  3  l�  ���                             7  �  T�      �Y      4   �����Y  �Y                         � ߱            $  8  (�  ���                       �Y                         � ߱        ع  $  <  ��  ���                       Ժ    ?  ��  �  \�  �Y      4   �����Y      $  @  0�  ���                       Z                         � ߱            �   ]  ,Z      lZ     
                �Z                     8\  @        
 �[              � ߱         �  V   q  p�  ���                        �  �   �  D\      �    #  0�  @�      �\      4   �����\      /   $  l�     |�                          3   �����\            ��                      3   �����\  �\     
                P]                     �^  @        
 `^              � ߱        ��  V   0  ��  ���                        �^     
                h_                     �`  @        
 x`              � ߱        ̼  V   T  <�  ���                        L�    �  �  d�      �`      4   �����`                t�                      ��                  �  �                  l�h	                       �  ��  �  /   �  ��     ��                          3   �����`            н                      3   �����`      /   �  �     �                          3   ����a            <�                      3   ����8a  ��  /  A  x�         la                      3   ����Ta  initProps   x�  ��              ,     Y     $                             �  	                                   ̿          t�  \�      ��                   2  ��              x�g	                    O   ����    e�          O   ����    R�          O   ����    ��      �                      ��          ��  p   $  �t  �      /  t�  d�     �t                                        ��                  %  A                  � g	                       %  ��   �  ��     �t                                        ��                  B  ^                  �!g	                       B  ��  ��  |�     �t                                        ��                  _  {                  t"g	                       _  �  �  �     u                                        ��                  |  �                  L\g	                       |  ��  ��  ��     u                                        ��                  �  �                  �\g	                       �  (�  0�   �     0u                                        ��                  �  �                  �]g	                       �  ��  ��  ��     Du                                        ��                  �  �                  `^g	                       �  @�  H�  8�     Xu                                        ��                  �                    0_g	                       �  ��  ��  ��     lu  	                                      ��             	       )                  8g	                         X�  `�  P�     �u  
                                      ��             
     *  F                  �8g	                       *  ��  ��  ��     �u                                        ��                  G  c                  �9g	                       G  p�  x�  h�     �u                                        ��                  d  �                  \:g	                       d  ��  �  ��     �u                                        ��                  �  �                  ,;g	                       �  ��  ��  ��     �u                                        ��                  �  �                  D�g	                       �  �  �  �     �u                                        ��                  �  �                  �g	                       �  ��  ��  ��     �u                                        ��                  �  �                  ��g	                       �  ,�  4�  $�     v                                        ��                  �                    ��g	                       �  ��      ��      v                                        ��                    .                  ��g	                         D�      O   1  ��  ��  4v               D�          ,�  8�   , �                                                       �     ��                            ����                            ��  d�  ��  �      ��     Z     L�                      � H�  �                     ��    G  �  ��      @v      4   ����@v                ��                      ��                  H  \                  Pig	                       H  �  ��  /   I  ��     ��                          3   ����Pv            ��                      3   ����pv  h�  /   J  (�     8�                          3   �����v            X�                      3   �����v  ��  /   O  ��     ��                          3   �����v            ��                      3   �����v      /   U   �     �                          3   ����w            0�                      3   ����$w  Dw     
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
   $�                      3   �����  d�        T�                      3   ����$�  ��        ��                      3   ����8�            ��                      3   ����T�  pushRowObjUpdTable  ��  ��  �                   [      �                               �                     pushTableAndValidate    ��  4�  �           |     \     �                          �                       remoteCommit    L�  ��  �           p     ]     �                          �  P                     serverCommit    ��  �  �           l     ^     �                          �  ]                                     4�          �  ��      ��                  
    �              ,�W	                    O   ����    e�          O   ����    R�          O   ����    ��          O     ��  ��  ��    ��                            ����                            $�  P�      ��              _      L�                      
�     j                     disable_UI  ��  ��                      `      �                               }  
                    �  �    ����  �       ��       ��  8   ����   ��  8   ����       8   ����       8   ����       ��  ��      viewObject  ,   ��  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  �  4�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  �  x�  ��      returnFocus ,INPUT hTarget HANDLE   h�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  ��  �      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  \�  l�      removeAllLinks  ,   L�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE p�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  t�  ��      hideObject  ,   d�  ��  ��      exitObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  ��  ��      displayLinks    ,   ��  �  �      createControls  ,   ��  (�  8�      changeCursor    ,INPUT pcCursor CHARACTER   �  d�  p�      applyEntry  ,INPUT pcField CHARACTER    T�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  �  �      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  h�  p�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE X�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  �      runServerObject ,INPUT phAppService HANDLE  ��  8�  L�      disconnectObject    ,   (�  `�  p�      destroyObject   ,   P�  ��  ��      bindServer  ,   t�  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  ��  �      startFilter ,   ��  �  (�      releaseDBRow    ,   �  <�  L�      refetchDBRow    ,INPUT phRowObjUpd HANDLE   ,�  x�  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE h�  ��  ��      fetchDBRowForUpdate ,   ��  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  (�  8�      compareDBRow    ,   �  L�  `�      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   <�  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  �  �      updateState ,INPUT pcState CHARACTER    ��  D�  X�      updateQueryPosition ,   4�  l�  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    \�  ��  ��      undoTransaction ,   ��  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  x�  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   h�  ��  �      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  P�  d�      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  @�  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  ,�  @�      startServerObject   ,   �  T�  d�      setPropertyList ,INPUT pcProperties CHARACTER   D�  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ��   �      rowObjectState  ,INPUT pcState CHARACTER    ��  ,�  <�      retrieveFilter  ,   �  P�  d�      restartServerObject ,   @�  x�  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   h�  ��  ��      refreshRow  ,   p�  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  �  $�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  �  T�  l�      initializeServerObject  ,   D�  ��  ��      initializeObject    ,   p�  ��  ��      home    ,   ��  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��   �  �      fetchPrev   ,   ��   �  ,�      fetchNext   ,   �  @�  L�      fetchLast   ,   0�  `�  l�      fetchFirst  ,   P�  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   p�  ��  ��      endClientDataRequest    ,   ��  ��  ��      destroyServerObject ,   ��  �  �      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    ��  h�  x�      dataAvailable   ,INPUT pcRelative CHARACTER X�  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  ��  �      commitTransaction   ,   ��   �  0�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    �  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 f	%     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        ��    :   %               � 
"    
 �%              h �P  \         (          
�                          
�            � [   L	
"    
 g	
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
 g	�           �
    1� <   g	� �   �%               o%   o           %              
"   
 g	�           `    1� D   g	� v   �%               o%   o           � {    g	
"   
 g	�           �    1� U   g	� v   �%               o%   o           o%   o           
"   
 g	�           P    1� e  
 g	� v   �%               o%   o           � {    g	
"   
 g	�           �    1� p   g	� �  	 �%               o%   o           � �  / g	
"   
 ��          8    1� �   �� �  	   
"   
 g	�           t    1� �   g	� �  	 �o%   o           o%   o           � {    g	
"   
 ��          �    1� �   �� �  	   
"   
 g	�           $    1� �   g	� �  	 �o%   o           o%   o           � {    g	
"   
 ��          �    1� �   �� �     
"   
 ��          �    1�    �� �  	   
"   
 ��              1�    �� �  	   
"   
 ��          L    1� '   �� �  	   
"   
 g	�           �    1� 5   g	� �   �o%   o           o%   o           %              
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
 g	�                1� �   g	� v   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 g	
"   
   
"   
 L	(�  L ( l       �        �    �� �   � P   �        �    �@    
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
 g	�           �    1�   
 g	� v   �%               o%   o           � {    g	
"   
 g	�           <    1� %  
 g	� v   �%               o%   o           o%   o           
"   
 h	�           �    1� 0   h	�    �%               o%   o           o%   o           
"   
 g	�           4    1� 9   g	� �   �%               o%   o           %               
"   
 g	�           �    1� H   g	� �   �%               o%   o           %               
"   
 f	�           ,    1� U   f	� v   �%               o%   o           � {    g	
"   
 g	�           �    1� \   g	� �   �%               o%   o           %              
"   
 g	�               1� n   g	� �   �%               o%   o           o%   o           
"   
 g	�           �    1� z   g	� v   �%               o%   o           o%   o           
"   
 h	�               1� �  	 h	� v   �%               o%   o           � {    g	
"   
 h	�           �    1� �   h	� v   �%               o%   o           o%   o           
"   
 g	�               1� �   g	� v   �%               o%   o           o%   o           
"   
 g	�           �    1� �   g	� �   �%               o%   o           %               
"   
 g	�           �    1� �   g	� �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 g	�           �    1� �  
 g	� �   �%               o%   o           %              
"   
 h	�           H    1� �   h	� v   �%               o%   o           o%   o           
"   
 g	�           �    1� �   g	� v   �%               o%   o           � {    g	
"   
 g	�           8    1� �   g	� v   �%               o%   o           o%   o           
"   
 ��          �    1�    ��      
"   
 g	�           �    1�    g	� v   �%               o%   o           � "  ! g	
"   
 g	�           d    1� D   g	� v   �%               o%   o           � {    g	
"   
 g	�           �    1� Q   g	� v   �%               o%   o           � d   g	
"   
 ��          L    1� s   �� �     
"   
 ��          �    1� �   ��      
"   
 g	�           �    1� �   g	� v   �%               o%   o           � {    h	
"   
 ��          8     1� �  
 ��      
"   
 f	�           t     1� �   f	� �   �%               o%   o           o%   o           
"   
 g	�           �     1� �   g	� �   �%               o%   o           %               
"   
 g	�           l!    1� �   g	� �   �%               o%   o           %               
"   
 g	�           �!    1� �   g	� v   �%               o%   o           � {    g	
"   
 g	�           \"    1� �   g	� v   �%               o%   o           o%   o           
"   
 g	�           �"    1� �   g	� �   �%               o%   o           %              
"   
 g	�           T#    1� 
   g	� �   �%               o%   o           %               
"   
 f	�           �#    1�    f	� �   �%               o%   o           %               
"   
 ��          L$    1� '   ��      
"   
 ��          �$    1� 4   �� v     
"   
 g	�           �$    1� A   g	� 8   �%               o%   o           o%   o           
"   
 g	�           @%    1� M   g	� v   �%               o%   o           � {    g	
"   
 g	�           �%    1� [   g	� v   �%               o%   o           o%   o           
"   
 h	�           0&    1� i   h	� �   �o%   o           o%   o           o%   o           
"   
 h	�           �&    1� ~   h	� �  	 �%               o%   o           o%   o           
"   
 g	�           ('    1� �   g	� v   �%               o%   o           o%   o           
"   
 h	�           �'    1� �  
 h	� 8   �%               o%   o           o%   o           
"   
 ��           (    1� �   �� v     
"   
 g	�           \(    1� �   g	� v   �%               o%   o           � �  4 g	
"   
 g	�           �(    1�   
 g	� �   �%               o%   o           %              
"   
 ��          L)    1�    ��      
"   
 g	�           �)    1�     g	� v   �%               o%   o           � {    f	
"   
 g	�           �)    1� .   g	� �   �%               o%   o           %              
"   
 g	�           x*    1� =   g	� v   �%               o%   o           � {    g	
"   
 g	�           �*    1� J   g	� v   �%               o%   o           � {    g	
"   
 g	�           `+    1� X   g	� v   �%               o%   o           � {    g	
"   
 g	�           �+    1� d   g	� �   �%               o%   o           %               
"   
 h	�           P,    1� s  	 h	�    �%               o%   o           o%   o           
"   
 f	�           �,    1� }   f	� v   �%               o%   o           � �  	 g	
"   
 g	�           @-    1� �   g	� 8   �%               o%   o           %       �       
"   
 g	�           �-    1� �   g	� v   �%               o%   o           � {    g	
"   
 g	�           0.    1� �   g	� �   �o%   o           o%   o           %              
"   
 g	�           �.    1� �   g	� �   �%               o%   o           %               
"   
 g	�           (/    1� �   g	� v   �%               o%   o           o%   o           
"   
 g	�           �/    1� �   g	� �  	 �%               o%   o           � {    g	
"   
 ��          0    1� �   �� �  	   P �L 
�H T   %              �     }        �GG %              
"   
 f	�           �0    1�   
 f	� v   �%               o%   o           � {    f	
"   
 h	�           1    1�    h	� �   �%               o%   o           %               
"   
 g	�           �1    1�   	 g	� v   �%               o%   o           � {    h	
"   
 g	�           2    1� #   g	� v   �%               o%   o           � {    g	
"   
 g	�           �2    1� 1   g	� �   �%               o%   o           %               
"   
 g	�           �2    1� A   g	� v   �%               o%   o           � {    g	
"   
 g	�           p3    1� T   g	� v   �%               o%   o           o%   o           
"   
 g	�           �3    1� \   g	� v   �%               o%   o           o%   o           
"   
 g	�           h4    1� i   g	� �   �%               o%   o           o%   o           
"   
 f	�           �4    1� w   f	� �   �%               o%   o           o%   o           
"   
 h	�           `5    1� �   h	� �   �%               o%   o           o%   o           
"   
 g	�           �5    1� �   g	� v   �%               o%   o           o%   o           
"   
 g	�           X6    1� �  	 g	� �  	 �%               o%   o           � {    g	
"   
 g	�           �6    1� �  
 g	� �  	 �%               o%   o           � {    g	
"   
 g	�           @7    1� �   g	� v   �%               o%   o           � {    g	
"   
 h	�           �7    1� �   h	� v   �%               o%   o           o%   o           
"   
 g	�           08    1� �   g	� v   �%               o%   o           o%   o           
"   
 g	�           �8    1� �   g	� v   �%               o%   o           � {    g	
"   
 g	�            9    1� �   g	� v   �%               o%   o           � {    g	
"   
 g	�           �9    1� 
   g	� �  	 �%               o%   o           o%   o           
"   
 ��          :    1�    ��      
"   
 g	�           L:    1� (   g	� v   �%               o%   o           � {    g	
"   
 g	�           �:    1� 6   g	� v   �%               o%   o           o%   o           
"   
 f	�           <;    1� I   f	� �   �%               o%   o           o%   o           
"   
 g	�           �;    1� [  
 g	� v   �%               o%   o           � {    g	
"   
 h	�           ,<    1� f   h	� v   �%               o%   o           � {    g	
"   
 g	�           �<    1� ~   g	� �   �%               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 g	�           p=    1� �  	 g	�    �%               o%   o           o%   o           
"   
 g	�           �=    1� �   g	�    �%               o%   o           o%   o           
"   
 h	�           h>    1� �   h	�    �%               o%   o           o%   o           
"   
 f	�           �>    1� �   f	� �   �%               o%   o           %              
"   
 g	�           `?    1� �   g	� v   �%               o%   o           � �  M f	
"   
 g	�           �?    1� 2   g	� �   �%               o%   o           %              
"   
 h	�           P@    1� C   h	� �   �%               o%   o           %               
"   
 g	�           �@    1� W   g	� �   �%               o%   o           %               
"   
 g	�           HA    1� n   g	� �  	 �%               o%   o           � |   g	
"   
 g	�           �A    1� �   g	� �   �%               o%   o           %               
"   
 h	�           8B    1� �   h	� �  	 �%               o%   o           o%   o           
"   
 g	�           �B    1� �   g	� �   �o%   o           o%   o           %              
"   
 f	�           0C    1� �   f	� �  	 �o%   o           o%   o           � {    f	
"   
 g	�           �C    1� �   g	�    �o%   o           o%   o           o%   o           
"   
 g	�            D    1� �   g	�    �o%   o           o%   o           o%   o           
"   
 g	�           �D    1� �   g	� �  	 �o%   o           o%   o           o%   o           
"   
 g	�           E    1� �   g	�    �o%   o           o%   o           o%   o           
"   
 g	�           �E    1�    g	� �  	 �o%   o           o%   o           �    g	
"   
 g	�           F    1�    g	� �  	 �o%   o           o%   o           � +   g	
"   
 g	�           |F    1� 7   g	� �   �%               o%   o           %               
"   
 g	�           �F    1� K   g	� �   �%               o%   o           %               
"   
 ��          tG    1� _   �� �  	   
"   
 g	�           �G    1� s   g	� �   �%               o%   o           %               
"   
 g	�           ,H    1�    g	� v   �%               o%   o           o%   o           
"   
 g	�           �H    1� �   g	� v   �%               o%   o           o%   o           
"   
 h	�           $I    1� �   h	� �   �%               o%   o           o%   o           
"   
 h	�           �I    1� �   h	� v   �%               o%   o           � {    g	
"   
 g	�           J    1� �   g	� �   �%               o%   o           %               
"   
 g	�           �J    1� �  	 g	� �   �%               o%   o           %                "    �%     start-super-proc v�%     adm2/smart.p -L	P �L 
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
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        �M    �� �   � P   �        �M    �@    
� @  , 
�       �M    �� �   L	p�               �L
�    %              � 8       N    � $         � �          
�    �    L	
"   
 �p� @  , 
�       O    �� 	   �p�               �L"  	  , �   �    g	�    ��     }        �A      |    "  	    �    g	%              (<   \ (    |    �     }        �A�    �A"  
  g	    "  	  L	"  
  g	  < "  	  L	"  
  g	(    |    �     }        �A�    �A"  
  g	
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   L	p�               �L
�    %              � 8      Q    � $         � �          
�    �    L	
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
 g	 (   � 
"   
 L	    �        �U    �� �   �
"   
   � 8      DV    � $         � �          
�    �    L	
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
   p�    � A   g	
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 L	    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 g	"      �       }        �
"   
 �%              %                "    �%     start-super-proc v�%     adm2/appserver.p �g	�    � �     
�    �     }        �%               %      Server  - �     }        �    "    g	� {    �%               %      Client      "    g	� {    �%      NONE    p�,  8         $     "    g	        � �   L	
�    
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        �Z    �� �   � P   �        �Z    �@    
� @  , 
�       �Z    �� �   L	p�               �L
�    %              � 8      �Z    � $         � �          
�    �    L	
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "    g	        � �   L	
�     "    �%     start-super-proc u�%     adm2/dataquery.p �g	
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
 L	(�  L ( l       �         ]    �� �   � P   �        ,]    �@    
� @  , 
�       8]    �� �   L	p�               �L
�    %              � 8      D]    � $         � �   L	     
�    �    L	
"   
 �p� @  , 
�       T^    �� x   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
 L	(�  L ( l       �        8_    �� �   � P   �        D_    �@    
� @  , 
�       P_    �� �   L	p�               �L
�    %              � 8      \_    � $         � �   L	     
�    �    L	
"   
 �p� @  , 
�       l`    �� �   �p�               �L%               "    �%     start-super-proc t�%     adm2/query.p -L	%     start-super-proc t�%     adm2/queryext.p % 	    initProps L	
�    %4 + $   FOR EACH gn-clie NO-LOCK INDEXED-REPOSITION �   � ^     � `     � b     
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        $b    �� �   � P   �        0b    �@    
� @  , 
�       <b    �� �   L	p�               �L
�    %              � 8      Hb    � $         � �          
�    �    L	
"   
 �p� @  , 
�       Xc    �� �   �p�               �L"    ,     "    g	� j    �
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        d    �� �   � P   �        (d    �@    
� @  , 
�       4d    �� �   L	p�               �L
�    %              � 8      @d    � $         � �          
�    �    L	
"   
 �p� @  , 
�       Pe    ��   	 �p�               �L"    , %               �    "      � `         %              %                   "      %                  "      "      "     T(        "    f	%              "    f	� `   �"      �       "    L	�    "    f	�    �� {      �    L	�    "     �     S    "      "    �    "    h	%                � @    �     t T     P   4       �"      (0       4       h	"      � {      � {    L	� ^   h	T ,  %              T   "    h	"    �� `     �    L	� ^   h	T    �    "    h	�    �"      �    L	"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    L	%              � {    �� k     4  d	     "      
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        j    �� �   � P   �        $j    �@    
� @  , 
�       0j    �� �   L	p�               �L
�    %              � 8      <j    � $         � �          
�    �    L	
"   
 �p� @  , 
�       Lk    ��   
 �p�               �L"    ,       "  
  g	�    � m  ! g	� `   �      "  	    �    � m  ! �� `   g	�   � ^     � `     � m  ! L	�   � ^     � `   L	� m  ! g	�   � ^     � `     � m  !   
�H T   %              �     }        �GG %              
"   
 �
"   
 L	
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
    �    � m  !   � `         "  	  d	�     "    h	T    "      "      @ A,    �   � ^   �� k     "    L	"       T      @   "    �(        "      � {    L	� {      � ^   L	"    d	     "  	   %              D H   @ A,    �   � ^   L	� k     "    L	"    g	,    S   "    L	� m  ! g	� `   �%                T      @   "    �(        "      � {    L	� {      � ^   L	"    g	     "  
   %                         "    �� k     "    L	           "      � k   L	"      
�H T   %              �     }        �GG %              
"   
 h	
"   
   
"   
 h	
"   
 L	(�  L ( l       �        �r    �� �   � P   �        �r    �@    
� @  , 
�       �r    �� �   h	p�               �L
�    %              � 8       s    � $         � �   L	     
�    �    �
"   
 �p� @  , 
�       t    �� f   �p�               �L"    , 
"   
   p� @  , 
�       ht    �� A     p�               �L"    , "      %              %              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    �%     start-super-proc t�%     adm2/data.p %     start-super-proc t�%     adm2/dataext.p 	%     start-super-proc t�%     adm2/dataextcols.p 	%     start-super-proc t�%     adm2/dataextapi.p d	
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
 L	(�  L ( l       �        �w    �� �   � P   �        �w    �@    
� @  , 
�       �w    �� �   L	p�               �L
�    %              � 8      �w    � $         � �   L	     
�    �    L	
"   
 �p� @  , 
�       �x    �� s   �p�               �L%               %     "Util/dgn-clie.i"   
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        �y    �� �   � P   �        �y    �@    
� @  , 
�       �y    �� �   L	p�               �L
�    %              � 8      �y    � $         � �          
�    �    L	
"   
 �p� @  , 
�       �z    �� n   �p�               �L"    , 
�     	         �G
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        |{    �� �   � P   �        �{    �@    
� @  , 
�       �{    �� �   L	p�               �L
�    %              � 8      �{    � $         � �          
�    �    L	
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
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        h}    �� �   � P   �        t}    �@    
� @  , 
�       �}    �� �   L	p�               �L
�    %              � 8      �}    � $         � �          
�    �    L	
"   
 �p� @  , 
�       �~    �� s  	 �p�               �L
"   
 , 
"   
 �     � �  	   �        �~    �
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        t    �� �   � P   �        �    �@    
� @  , 
�       �    �� �   L	p�               �L
�    %              � 8      �    � $         � �          
�    �    L	
"   
 �p� @  , 
�       ��    �� �   �p�               �L"    , 
"   
   �        �    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 L	
"   
 �
"   
 L	
"   
   (�  L ( l       �        ��    �� �   � P   �        ��    �@    
� @  , 
�       ��    �� �   L	p�               �L
�    %              � 8      ��    � $         � �          
�    �    L	
"   
 �p� @  , 
�       ��    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 g	        � �   L	
�    
�             �Gp�,  8         $     
"   
 g	        � �   L	
�    �    � �     
�        "    g	� {    �%     modifyListProperty 
�    %      REMOVE  %     SupportedLinks 	%     Update-Target  	%     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � 9     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   l       ��                 ^  �  �               �g	                    O   ����    e�          O   ����    R�          O   ����    ��        $  m  �   ���                       �U     
                    � ߱              n  (  �      V      4   ����V                �                      ��                  o  �                  �(g	                       o  8  �  �  p  PV            r  �  `      �V      4   �����V                p                      ��                  s  �                  P)g	                       s  �  �  o   t      ,                                 �  �   u  �V      �  �   v  �V      $  $  w  �  ���                        W     
                    � ߱        8  �   x  @W      L  �   y  `W      `  �   |  �W          $     �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               �*g	                    O   ����    e�          O   ����    R�          O   ����    ��      b                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  D�g	                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��                 M    �               ��h	                    O   ����    e�          O   ����    R�          O   ����    ��        $  m  �   ���                       ta                         � ߱        �  $  n  8  ���                       �a                         � ߱        �a     
                Tb                     �c  @        
 dc              � ߱        �  V   �  d  ���                        �    �    �      �c      4   �����c  �c     
                Ld                     �e  @        
 \e              � ߱            V   �     ���                          $    �  ���                       �e                         � ߱        �  $    4  ���                       �e                         � ߱          �      4  8                      ��        0           1                  �h	      Tf     �       `      $      ���                       �e                         � ߱        �  $    `  ���                       f                         � ߱            4   ����4f  `f                     �f                     �f                     g                     $g                         � ߱        d  $    �  ���                             )  �  �      Dg      4   ����Dg      $  *  �  ���                       lg          �h             � ߱        �  $  4    ���                       �h                         � ߱          �        |                      ��        0         6  ;                  �h	      8i     8     6  @      $  6  �  ���                       �h                         � ߱        l  $  6  @  ���                       �h                         � ߱            4   ����i      $  8  �  ���                       Li                         � ߱        �i     
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
        |                      ��        0         �  �                  ��g	      �o     T     �  0
      $  �  �
  ���                       o                         � ߱        \  $  �  0  ���                       Ho                         � ߱        l  4   ����po      4   �����o  �  $  �  �  ���                       p                         � ߱        �    �  �  l      0p      4   ����0p                �                      ��                  �  �                  ��g	                       �     tp                     �p       	       	           � ߱            $  �  |  ���                             �    �      q      4   ����q                �                      ��                  �  �                  �g	                       �    �q                      r       
       
           � ߱            $  �  �  ���                       (r                     \r                         � ߱           $  �    ���                       �r     
                s                     \t  @        
 t          �t  @        
 tt              � ߱            V   �  �  ���                                    7 �          �  d  � `                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             �   �   �   �   �   �   �   �       $  4  D  T  d  t  �  \  �  �  �  �  8HXhx��������(8HX      �   �   �   �   �   �   �   �      $  4  D  T  d  t  �   \ �  �  �  �  8HXhx��������(8HX  �                    � �                     �          ��                            ����                                                        �   l       ��                  y  �  �               <�h	                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                  �  �  �               �h	                    O   ����    e�          O   ����    R�          O   ����    ��      �       �              �                  $                  d  /  �  $     4  ��                      3   ����p�            T                      3   ������      O   �  ��  ��  ��               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  �  �  �               �h	                    O   ����    e�          O   ����    R�          O   ����    ��             �              �                $                  $       ,             �          /                                �  /  �  t     �  Ȅ                      3   ������            �                      3   ����Є     /  �  �     �  ��                      3   ����܄  x                             3   ���� �      $   �  L  ���                                                   � ߱                  �  �                  3   �����      $   �  �  ���                                                   � ߱        X  $  �  ,  ���                       �                         � ߱            O   �  ��  ��  4�               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                  �  �  �               ��X	                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  $                    �          /                      �              /  �  L     \  d�                      3   ����H�  �        |  �                  3   ����l�      $   �  �  ���                                                   � ߱                                      3   ����x�      $   �  @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                  V  a  �               �X	                    O   ����    e�          O   ����    R�          O   ����    ��            `  �   �       ��      4   ������      �   `  ��    ��                            ����                            TXS appSrvUtils O:\on_in_co\Util\dgn-clie.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "Util/dgn-clie.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server Client NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH gn-clie NO-LOCK INDEXED-REPOSITION ,   gn-clie  ; NomCli DirCli NroCard Ruc CodDept INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p NomCli DirCli NroCard Ruc CodDept RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery x  �-  �  @;      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
   RowObject   <         D         L         T         X         `         h         t         |         �         NomCli  DirCli  NroCard Ruc CodDept RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �  �     RowObjUpd   4         <         D         L         P         X         `         l         t         �         �         NomCli  DirCli  NroCard Ruc CodDept RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   �          �  
   appSrvUtils �       �     xiRocketIndexLimit          �  
   gshAstraAppserver   4           
   gshSessionManager   X        H  
   gshRIManager    �        l  
   gshSecurityManager  �        �  
   gshProfileManager   �        �  
   gshRepositoryManager       	 	     �  
   gshTranslationManager   $  
 
       
   gshWebManager   H        8     gscSessionId    l        \     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager   �        �     gsdTempUniqueID              gsdUserObj  D        0     gsdRenderTypeObj    l        X     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf   �       �     glADMLoadFromRepos              glADMOk 0       $  
   ghContainer P    	   D     cObjectName l    
   d     iStart  �       �     cAppService �       �     cASDivision �       �     cServerOperatingMode    �       �     cContainerType               cQueryString    @       4  
   hRowObject  `       T  
   hDataQuery  �       t     cColumns             �     cDataFieldDefs  �    X  �  RowObject         X  �  RowObjUpd          "   >   �   �   �   �          !  8  D  E  F  H  J  K  L  P  Q  T  U  V  W  Y  [  ]  _  `  a  d  f  g  i  j  k  l  m  s  u  {  }    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
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
        ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  -  �  �  �  �  �  �  �  �  �      0  O  Q  f  �      !  1  2  3  6  7  8  <  ?  @  ]  q  �  #  $  0  T  �  �  �  �  �  A  G  H  I  J  O  U  \  �  �  �  �  �      2  <  V  W  a  {  �  �  �  �  �  �      ;�  O:\on_in_co\Util\dgn-clie.w  #  ��  C:\Progress\OpenEdge\src\adm2\data.i 0#  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    `#  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i �#  lo , O:\on_in_co\Util\dgn-clie.i  �#  �:   C:\Progress\OpenEdge\src\adm2\query.i    �#  z + C:\Progress\OpenEdge\src\adm2\delrecst.i 0$  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  d$   ) %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   �$  � ! C:\Progress\OpenEdge\src\adm2\dataquery.i    �$  �Z ( %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   %  �< " C:\Progress\OpenEdge\src\adm2\appserver.i    T%  �� ' %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �%  I� # C:\Progress\OpenEdge\src\adm2\smart.i    �%  Ds & C:\Progress\OpenEdge\gui\fn  &  tw % %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   ,&  Q. $ C:\Progress\OpenEdge\gui\set l&  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i �&  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    �&  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    '  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  P'  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i �'  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i �'   
 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    (  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   @(  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   �(  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �(  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    )  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    H)  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �)  �j  C:\Progress\OpenEdge\gui\get �)  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �)  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    ,*  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i p*  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �*  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �*  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   +  �  C:\Progress\OpenEdge\src\adm2\appsprto.i \+  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �+  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �+  ª  %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   ,  �� 	 C:\Progress\OpenEdge\src\adm2\qryprto.i  \,  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   �,  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �,  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    -  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   L-  �.    O:\on_in_co\Util\dgn-clie_cl.w          g      �-  �        �-  [  �     �-     �  &   �-  �   E     �-     �  .    .  �   �     .     �      .  �   �     0.     �  $   @.  �   �     P.     }  $   `.  �   {     p.     Y  $   �.  �   V     �.     4  $   �.  �   2     �.       $   �.  �        �.     �  $   �.  �   �     �.     �  $    /  �   �     /     �  $    /  �   �     0/     ~  -   @/  �   z     P/     t  ,   `/  k   :     p/  �  .      �/       +   �/  �        �/     �  +   �/  �  �      �/     �  +   �/  �  �      �/     �  +   �/  �  �       0     �  +   0  �  �       0     �  +   00  �  �      @0     f  +   P0  �  c      `0     I  +   p0  �  F      �0     ,  +   �0  �  )      �0       +   �0  �        �0     �  +   �0  �  �      �0     �  +   �0  �  �       1     �  +   1  �  �       1     �  +   01  �  �      @1     ~  +   P1  �  {      `1     a  +   p1  �  ^      �1     D  +   �1  �  A      �1     '  +   �1  �        �1     �  $   �1  �  �      �1     �  $   �1  k  �       2     {  $   2  j  z       2     X  $   02  i  W      @2     5  $   P2  _  +      `2       *   p2  ^        �2     �  *   �2  ]  �      �2     �  *   �2  \  �      �2     �  *   �2  [  �      �2     i  *   �2  Z  h       3     B  *   3  Y  A       3       *   03  X        @3     �  *   P3  W  �      `3     �  *   p3  V  �      �3     �  *   �3  U  �      �3       *   �3  T  ~      �3     X  *   �3  S  W      �3     1  *   �3  R  0       4     
  *   4  Q  	       4     �  *   04  P  �      @4     �  *   P4  O  �      `4     �  *   p4  N  �      �4     n  *   �4  @  `      �4     >  $   �4          �4     �  $   �4    �      �4     �  $   �4  �   =       5     �  )   5  g   �       5  a   �  !   05     p  (   @5  _   n  !   P5     L  $   `5  ]   J  !   p5     (  $   �5  I     !   �5  �     "   �5     �  '   �5  �   �  "   �5     �  $   �5  �   �  "   �5     i  $   �5  �   g  "    6     E  $   6  g   +  "    6          06  O   �  "   @6  �   ~  #   P6     |  &   `6  �   L  #   p6     �  %   �6  �   �  #   �6     �  $   �6  �   �  #   �6     �  $   �6  �   �  #   �6     �  $   �6  �   �  #   �6     ^  $    7  �   J  #   7     (  $    7  }     #   07     �  $   @7     ~  #   P7     0  "   `7     �  !   p7     �      �7     6     �7  �   -     �7  O        �7          �7     �     �7  �   �     �7  �   ~     �7  O   p      8     _     8           8  y   �
     08  �   �
  
   @8  G   �
     P8     �
     `8     y
     p8  c   
  
   �8  x   
     �8  M   �	     �8     �	     �8     �	     �8  a   �	     �8  �  g	     �8     H	     �8  �  	      9  O   	     9     �      9     �     09  �   �     @9     �     P9     �     `9  x   �     p9     �     �9     c     �9     _     �9     K     �9     2     �9  Q   "     �9     �     �9     �     �9     |      :     b     :  ]   \  
    :     R     0:     
  
   @:     �     P:     �  
   `:  Z   �     p:     �  	   �:     �     �:     �     �:     �     �:  c   f     �:     D     �:     �      �:     �      �:     �       ;     �      ;     &       ;           0;           