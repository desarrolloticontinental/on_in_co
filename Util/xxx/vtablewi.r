	��VX�jR ?    �              �                                 �� 3F000143utf-8 MAIN C:\newsie\on_in_co\util\xxx\vtablewi.w,, PROCEDURE disable_UI,, PROCEDURE displayObjects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER PROCEDURE validateFields,,INPUT-OUTPUT pcNotValidFields CHARACTER PROCEDURE updateTitle,, PROCEDURE updateRecord,, PROCEDURE updateMode,,INPUT pcMode CHARACTER PROCEDURE showDataMessagesProcedure,,OUTPUT pcReturn CHARACTER PROCEDURE resetRecord,, PROCEDURE queryPosition,,INPUT pcState CHARACTER PROCEDURE okToContinueProcedure,,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE deleteRecord,, PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE confirmDelete,,INPUT-OUTPUT plAnswer LOGICAL PROCEDURE confirmContinue,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE collectChanges,,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER PROCEDURE viewRecord,, PROCEDURE valueChanged,, PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE initializeObject,, PROCEDURE enableFields,, PROCEDURE displayFields,,INPUT pcColValues CHARACTER PROCEDURE disableFields,,INPUT pcFieldType CHARACTER PROCEDURE copyRecord,, PROCEDURE cancelRecord,, PROCEDURE addRecord,, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION showDataMessages,CHARACTER, FUNCTION setWindowTitleField,LOGICAL,INPUT cWindowTitleField CHARACTER FUNCTION setUpdateTargetNames,LOGICAL,INPUT pcTargetNames CHARACTER FUNCTION setUpdateTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setTableIOSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setTableIOSource,LOGICAL,INPUT phObject HANDLE FUNCTION setSaveSource,LOGICAL,INPUT plSave LOGICAL FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setLogicalObjectName,LOGICAL,INPUT pcLogicalObjectName CHARACTER FUNCTION setGroupAssignTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setGroupAssignSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignSource,LOGICAL,INPUT phObject HANDLE FUNCTION setEnabledFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDisplayedFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDataModified,LOGICAL,INPUT plModified LOGICAL FUNCTION setContainerMode,LOGICAL,INPUT pcContainerMode CHARACTER FUNCTION okToContinue,LOGICAL,INPUT pcAction CHARACTER FUNCTION getWindowTitleField,CHARACTER, FUNCTION getUpdateTargetNames,CHARACTER, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getTableIOSourceEvents,CHARACTER, FUNCTION getTableIOSource,HANDLE, FUNCTION getRowIdent,CHARACTER, FUNCTION getRecordState,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getNewRecord,CHARACTER, FUNCTION getGroupAssignTargetEvents,CHARACTER, FUNCTION getGroupAssignTarget,CHARACTER, FUNCTION getGroupAssignSourceEvents,CHARACTER, FUNCTION getGroupAssignSource,HANDLE, FUNCTION getFieldsEnabled,LOGICAL, FUNCTION getFieldHandles,CHARACTER, FUNCTION getEnabledHandles,CHARACTER, FUNCTION getEnabledFields,CHARACTER, FUNCTION getDisplayedTables,CHARACTER, FUNCTION getDisplayedFields,CHARACTER, FUNCTION getDataModified,LOGICAL, FUNCTION getCreateHandles,CHARACTER, FUNCTION setShowPopup,LOGICAL,INPUT plShowPopup LOGICAL FUNCTION getShowPopup,LOGICAL, FUNCTION getObjectType,character, FUNCTION getTargetProcedure,HANDLE, FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER      �              �	             � �  �              �o              �*    +   $y �  U   �} `  V   $� �   Z   � t  ]           �� �  ? ,� �$  iSO8859-1                                                                           �    �                                      �                   �                    �         �t    �             ��  �   P      \                                                         PROGRESS                         �           
    
                    �              �                                                                                                     
           �          X  �     �     �      �                       �          D      �   �             l                                                                                          �  ��         ,  �      �  
    
                  �  \                                                                                                       �          
  �  �      T  
    
                  @               �                                                                                          �          
  �  �         
    
                  �  �             p                                                                                          �          
  0  �      �  
    
                  �  `                                                                                                       �          
  �  �      X  
    
                  D               �                                                                                          �          
  �          
    
                  �  �  	           t                                                                                                    
  4  %      �  
    
                  �  d  
                                                                                                      %          
  �  ;      \  
    
                  H               �                                                                                          ;          
  �  I                               �  �             x                                                                                          I            8	  V      �                        �  h	             $	                                                                                          V            �	  d      `	  
    
                  L	  
             �	                                                                                          d          
  �
  r      
  
    
                  �	  �
             |
                                                                                          r          
  <  �      �
  
    
                  �
  l             (                                                                                          �          
  �  �      d                        P               �                                                                                          �            �  �                              �  �             �                                                                                          �            @  �      �                        �  p             ,                                                                                          �                �      h                        T                 �                                                                                          �                          t�                                               x�          d  �  H XT            
             
             
                                         
                                                                                                                                                                        H   X   h   x   �   �   �   �   �   �   �   �       (  8  H      H   X   h   x   �   �   �   �   �   �   �   �      (  8  H                                                                            �  �                             ,  8  @  X  L          \             t  |  �  �  �          �                                                         CodAlm  x(3)    Almac�n Almac�n     C�digo de almac�n   Descripcion X(40)   Descripci�n Descripci�n     Descripci�n de almac�n  CodCia  999 Cia Cia 0   C�digo de Compa�ia  �  ���������   �       �$                �     i     	       !   -     ��                                               �          ����                            undefined                                                               �           �   l                             �����               @�                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     D          assignFocusedWidget         �      �     4       LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    H       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    Z       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          p       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    |       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    �       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |          LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �          LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    "      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 /      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    :      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    G      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    [      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    i      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    y      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    �      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    d�    =  �
        d       4   ����d                                       ��                  =  A                  4D                       =  �
  \  	  >  L                                        3   ����|       O   @  ��  ��  �   addRecord                                 �      ��                  �  �                8]�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                  �      ��                  �  �                 h
�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyRecord                                �      ��                  �  �                 �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableFields                                 �      ��                  �  �  $              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  <           ��                            ����                            displayFields                               8         ��                  �  �  P              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  h           ��                            ����                            enableFields                                d  L      ��                  �  �  |              4�}                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                l  T      ��                      �              Զ}                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            toolbar                             h  P      ��                      �              P�h                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            updateState                             �  x      ��                    
  �              (L�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            valueChanged                                �  �      ��                      �              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewRecord                              �  �      ��                      �              h��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            getTargetProcedure  
      ,      `    =      HANDLE, getObjectType   @      h      �    P      CHARACTER,  getShowPopup    x      �      �    ^      LOGICAL,    setShowPopup    �      �          k      LOGICAL,INPUT plShowPopup LOGICAL   addRecord                               �  �      ��                  �  �  �              �?�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                �  �      ��                  �  �  �              �@�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            collectChanges                              �  �      ��                  �  �  �              W�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   0             �               ��                  $           ��                            ����                            confirmContinue                                      ��                  �  �  8              �W�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  P           ��                            ����                            confirmDelete                               L  4      ��                  �  �  d              @,^                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  |           ��                            ����                            confirmExit                             t  \      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            copyRecord                              �  �      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            dataAvailable                               �   �       ��                  �  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �            ��                            ����                            deleteRecord                                �!  �!      ��                  �  �  �!              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �"  �"      ��                  �  �  �"              0��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            okToContinueProcedure                               �#  �#      ��                  �  �  �#              P��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   D$             $               ��                  8$           ��                            ����                            queryPosition                               4%  %      ��                  �  �  L%              d�}                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d%           ��                            ����                            resetRecord                             \&  D&      ��                  �  �  t&              x��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            showDataMessagesProcedure                               l'  T'      ��                  �  �  �'              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �'           ��                            ����                            updateMode                              �(  |(      ��                  �  �  �(              S�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �(           ��                            ����                            updateRecord                                �)  �)      ��                  �  �  �)              TW�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �*  �*      ��                  �  �  �*              8]�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �*           ��                            ����                            updateTitle                             �+  �+      ��                  �  �   ,              @b�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            validateFields                              �,  �,      ��                  �  �  -              s�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  -           ��                            ����                            getCreateHandles    �      �-      �-    x      CHARACTER,  getDataModified �-      �-      �-    �      LOGICAL,    getDisplayedFields  �-       .      4.    �      CHARACTER,  getDisplayedTables  .      @.      t.    �      CHARACTER,  getEnabledFields    T.      �.      �.     �      CHARACTER,  getEnabledHandles   �.      �.      �.  !  �      CHARACTER,  getFieldHandles �.       /      0/  "  �      CHARACTER,  getFieldsEnabled    /      </      p/  #  �      LOGICAL,    getGroupAssignSource    P/      |/      �/  $        HANDLE, getGroupAssignSourceEvents  �/      �/      �/  %        CHARACTER,  getGroupAssignTarget    �/      0      <0  &  3      CHARACTER,  getGroupAssignTargetEvents  0      H0      �0  '  H      CHARACTER,  getNewRecord    d0      �0      �0  (  c      CHARACTER,  getObjectParent �0      �0      �0  )  p      HANDLE, getRecordState  �0      1      41  *  �      CHARACTER,  getRowIdent 1      @1      l1  +  �      CHARACTER,  getTableIOSource    L1      x1      �1  ,  �      HANDLE, getTableIOSourceEvents  �1      �1      �1  -  �      CHARACTER,  getUpdateTarget �1      �1      (2  .  �      CHARACTER,  getUpdateTargetNames    2      42      l2  /  �      CHARACTER,  getWindowTitleField L2      x2      �2  0  �      CHARACTER,  okToContinue    �2      �2      �2  1  �      LOGICAL,INPUT pcAction CHARACTER    setContainerMode    �2      3      @3  2  	      LOGICAL,INPUT pcContainerMode CHARACTER setDataModified  3      h3      �3  3        LOGICAL,INPUT plModified LOGICAL    setDisplayedFields  x3      �3      �3  4  *      LOGICAL,INPUT pcFieldList CHARACTER setEnabledFields    �3      4      H4  5  =      LOGICAL,INPUT pcFieldList CHARACTER setGroupAssignSource    (4      l4      �4  6  N      LOGICAL,INPUT phObject HANDLE   setGroupAssignSourceEvents  �4      �4       5  7  c      LOGICAL,INPUT pcEvents CHARACTER    setGroupAssignTarget    �4      $5      \5  8  ~      LOGICAL,INPUT pcObject CHARACTER    setGroupAssignTargetEvents  <5      �5      �5  9  �      LOGICAL,INPUT pcEvents CHARACTER    setLogicalObjectName    �5      �5      6  :  �      LOGICAL,INPUT pcLogicalObjectName CHARACTER setObjectParent �5      D6      t6  ;  �      LOGICAL,INPUT phParent HANDLE   setSaveSource   T6      �6      �6  <  �      LOGICAL,INPUT plSave LOGICAL    setTableIOSource    �6      �6      7  =  �      LOGICAL,INPUT phObject HANDLE   setTableIOSourceEvents  �6      87      p7  >  �      LOGICAL,INPUT pcEvents CHARACTER    setUpdateTarget P7      �7      �7  ?  	      LOGICAL,INPUT pcObject CHARACTER    setUpdateTargetNames    �7      �7       8  @        LOGICAL,INPUT pcTargetNames CHARACTER   setWindowTitleField  8      H8      |8  A  .      LOGICAL,INPUT cWindowTitleField CHARACTER   showDataMessages    \8      �8      �8  B  B      CHARACTER,  assignPageProperty                              �9  h9      ��                  �  �  �9              
]                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �9             �9               ��                  �9           ��                            ����                            changePage                              �:  �:      ��                  �  �  �:              t]                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �;  �;      ��                  �  �  �;              l��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                   <           ��                            ����                            constructObject                             �<  �<      ��                  �  �  =              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `=             ,=               �� 
  �=             T=  
             ��   �=             |=               �� 
                 �=  
         ��                            ����                            createObjects                               �>  �>      ��                  �  �  �>              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �?  �?      ��                  �  �  �?              T�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �?           ��                            ����                            destroyObject                               �@  �@      ��                  �  �  �@              L��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �A  �A      ��                       �A              䈑                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �A           ��                            ����                            initializeObject                                �B  �B      ��                      C              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               D  �C      ��                      $D              L�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               E  �D      ��                  
    $E              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  <E           ��                            ����                            notifyPage                              4F  F      ��                      LF              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  dF           ��                            ����                            passThrough                             \G  DG      ��                      tG              ("�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �G             �G               ��                  �G           ��                            ����                            removePageNTarget                               �H  �H      ��                      �H              Ŕ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  I             �H  
             ��                  I           ��                            ����                            selectPage                              J  �I      ��                      J              �ʔ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  4J           ��                            ����                            toolbar                             (K  K      ��                     "  @K              lu�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  XK           ��                            ����                            viewObject                              PL  8L      ��                  $  %  hL              lz�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                PM  8M      ��                  '  )  hM              �Р                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �M           ��                            ����                            disablePagesInFolder    �8      �M       N  C  S      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder  N      LN      �N  D  h      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  `N      �N      �N  E  |      HANDLE, getCallerWindow �N      �N      O  F  �      HANDLE, getContainerMode    �N       O      TO  G  �      CHARACTER,  getContainerTarget  4O      `O      �O  H  �      CHARACTER,  getContainerTargetEvents    tO      �O      �O  I  �      CHARACTER,  getCurrentPage  �O      �O      P  J  �      INTEGER,    getDisabledAddModeTabs  �O      $P      \P  K  �      CHARACTER,  getDynamicSDOProcedure  <P      hP      �P  L        CHARACTER,  getFilterSource �P      �P      �P  M        HANDLE, getMultiInstanceActivated   �P      �P       Q  N  )      LOGICAL,    getMultiInstanceSupported    Q      ,Q      hQ  O  C      LOGICAL,    getNavigationSource HQ      tQ      �Q  P  ]      CHARACTER,  getNavigationSourceEvents   �Q      �Q      �Q  Q  q      CHARACTER,  getNavigationTarget �Q      �Q      0R  R  �      HANDLE, getOutMessageTarget R      8R      lR  S  �      HANDLE, getPageNTarget  LR      tR      �R  T  �      CHARACTER,  getPageSource   �R      �R      �R  U  �      HANDLE, getPrimarySdoTarget �R      �R      S  V  �      HANDLE, getReEnableDataLinks    �R      $S      \S  W  �      CHARACTER,  getRunDOOptions <S      hS      �S  X  �      CHARACTER,  getRunMultiple  xS      �S      �S  Y  	      LOGICAL,    getSavedContainerMode   �S      �S      T  Z        CHARACTER,  getSdoForeignFields �S      $T      XT  [  .      CHARACTER,  getTopOnly  8T      dT      �T  \ 
 B      LOGICAL,    getUpdateSource pT      �T      �T  ]  M      CHARACTER,  getWaitForObject    �T      �T      U  ^  ]      HANDLE, getWindowTitleViewer    �T      U      LU  _  n      HANDLE, getStatusArea   ,U      TU      �U  `  �      LOGICAL,    pageNTargets    dU      �U      �U  a  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �U      �U      (V  b  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  V      @V      tV  c  �      LOGICAL,INPUT h HANDLE  setCallerWindow TV      �V      �V  d  �      LOGICAL,INPUT h HANDLE  setContainerTarget  �V      �V      W  e  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �V      ,W      \W  f  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  <W      xW      �W  g  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �W      �W      X  h  
      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �W      8X      hX  i  !      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  HX      �X      �X  j  1      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �X      �X      Y  k  D      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �X      HY      �Y  l  ^      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource dY      �Y      �Y  m  x      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �Y      Z      HZ  n  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget (Z      lZ      �Z  o  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �Z      �Z      �Z  p  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �Z      [      D[  q  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   $[      h[      �[  r  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget x[      �[      �[  s  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �[      \      L\  t  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget ,\      x\      �\  u  	      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �\      �\      �\  v  $	      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �\      ]      L]  w  4	      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   ,]      p]      �]  x  C	      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �]      �]      ^  y  Y	      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �]      4^      `^  z 
 m	      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource @^      �^      �^  {  x	      LOGICAL,INPUT pcSource CHARACTER    setWaitForObject    �^      �^      _  |  �	      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �^      (_      `_  }  �	      LOGICAL,INPUT phViewer HANDLE   setStatusArea   @_      �_      �_  ~  �	      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             d`  L`      ��                  �  �  |`              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               ha  Pa      ��                  �  �  �a              (��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                lb  Tb      ��                  �  �  �b              �`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                tc  \c      ��                  �  �  �c              D`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               xd  `d      ��                  �  �  �d              (R�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �d           ��                            ����                            getAllFieldHandles  �_      e      De    �	      CHARACTER,  getAllFieldNames    $e      Pe      �e  �  �	      CHARACTER,  getCol  de      �e      �e  �  �	      DECIMAL,    getDefaultLayout    �e      �e      �e  �  �	      CHARACTER,  getDisableOnInit    �e      f      8f  �  �	      LOGICAL,    getEnabledObjFlds   f      Df      xf  �  	
      CHARACTER,  getEnabledObjHdls   Xf      �f      �f  �  
      CHARACTER,  getHeight   �f      �f      �f  � 	 -
      DECIMAL,    getHideOnInit   �f      �f      ,g  �  7
      LOGICAL,    getLayoutOptions    g      8g      lg  �  E
      CHARACTER,  getLayoutVariable   Lg      xg      �g  �  V
      CHARACTER,  getObjectEnabled    �g      �g      �g  �  h
      LOGICAL,    getObjectLayout �g      �g      (h  �  y
      CHARACTER,  getRow  h      4h      \h  �  �
      DECIMAL,    getWidth    <h      hh      �h  �  �
      DECIMAL,    getResizeHorizontal th      �h      �h  �  �
      LOGICAL,    getResizeVertical   �h      �h      i  �  �
      LOGICAL,    setAllFieldHandles  �h       i      Ti  �  �
      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    4i      ti      �i  �  �
      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �i      �i      �i  �  �
      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �i       j      Tj  �  �
      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   4j      tj      �j  �        LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �j      �j      �j  �        LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �j      k      Lk  �  $      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal ,k      pk      �k  �  4      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �k      �k      l  �  H      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �k      ,l      `l  �  Z      LOGICAL,    getObjectSecured    @l      ll      �l  �  n      LOGICAL,    createUiEvents  �l      �l      �l  �        LOGICAL,    bindServer                              xm  `m      ��                  �  �  �m              �It                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               |n  dn      ��                  �  �  �n              LJt                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �o  lo      ��                  �  �  �o              
�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �p  tp      ��                  �  �  �p              �
�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �q  �q      ��                  �  �  �q              0�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �r  �r      ��                  �  �  �r              (�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �s  �s      ��                  �  �  �s              ؂                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �s  
         ��                            ����                            startServerObject                               �t  �t      ��                  �  �  �t              D�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �u  �u      ��                  �  �  �u              (�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  v           ��                            ����                            getAppService   �l      pv      �v  �  �      CHARACTER,  getASBound  �v      �v      �v  � 
 �      LOGICAL,    getAsDivision   �v      �v      w  �  �      CHARACTER,  getASHandle �v       w      Lw  �  �      HANDLE, getASHasStarted ,w      Tw      �w  �  �      LOGICAL,    getASInfo   dw      �w      �w  � 	 �      CHARACTER,  getASInitializeOnRun    �w      �w       x  �  �      LOGICAL,    getASUsePrompt  �w      x      <x  �  �      LOGICAL,    getServerFileName   x      Hx      |x  �  �      CHARACTER,  getServerOperatingMode  \x      �x      �x  �        CHARACTER,  runServerProcedure  �x      �x       y  �  (      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �x      Dy      ty  �  ;      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   Ty      �y      �y  �  I      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �y      �y      z  �  W      LOGICAL,INPUT phASHandle HANDLE setASInfo   �y      <z      hz  � 	 c      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    Hz      �z      �z  �  m      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �z      �z      {  �  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �z      4{      h{  �  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  H{      �{      �{  �  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �|  h|      ��                  s  w  �|              \ҁ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �|             �|  
             ��   }             �|               �� 
                  }  
         ��                            ����                            addMessage                              �}  �}      ��                  y  }  ~              lف                    O   ����    e�          O   ����    R�          O   ����    ��            ��   \~             (~               ��   �~             P~               ��                  x~           ��                            ����                            adjustTabOrder                              t  \      ��                    �  �              x�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             �  
             �� 
   �             �  
             ��                  �           ��                            ����                            applyEntry                              �  Ԁ      ��                  �  �  �              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            changeCursor                                �   �      ��                  �  �  0�              \�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  H�           ��                            ����                            createControls                              D�  ,�      ��                  �  �  \�              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               H�  0�      ��                  �  �  `�              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                L�  4�      ��                  �  �  d�              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              X�  @�      ��                  �  �  p�              X��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              X�  @�      ��                  �  �  p�              �ɧ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              X�  @�      ��                  �  �  p�              �ʧ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                `�  H�      ��                  �  �  x�              �ͧ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              h�  P�      ��                  �  �  ��              �Χ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  ̊             ��  
             ��   �             ��               ��   �             �               ��                  �           ��                            ����                            modifyUserLinks                             �  �      ��                  �  �  $�              ��{                    O   ����    e�          O   ����    R�          O   ����    ��            ��   p�             <�               ��   ��             d�               �� 
                 ��  
         ��                            ����                            removeAllLinks                              ��  p�      ��                  �  �  ��              ߋ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              ��  p�      ��                  �  �  ��              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             ��  
             ��   �             ��               �� 
                 �  
         ��                            ����                            repositionObject                                �  ��      ��                  �  �   �              �,�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   l�             8�               ��                  `�           ��                            ����                            returnFocus                             X�  @�      ��                  �  �  p�              �-�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ��  
         ��                            ����                            showMessageProcedure                                ��  t�      ��                  �  �  ��              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             ��               ��                  �           ��                            ����                            toggleData                              ܓ  ē      ��                  �  �  ��              $�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            viewObject                              �  �      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �{      t�      ��  � 
       LOGICAL,    assignLinkProperty  ��      ��      ��  �        LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   ��      8�      h�  �  &      CHARACTER,  getChildDataKey H�      t�      ��  �  4      CHARACTER,  getContainerHandle  ��      ��      �  �  D      HANDLE, getContainerHidden  Ė      �       �  �  W      LOGICAL,    getContainerSource   �      ,�      `�  �  j      HANDLE, getContainerSourceEvents    @�      h�      ��  �  }      CHARACTER,  getContainerType    ��      ��      �  �  �      CHARACTER,  getDataLinksEnabled ė      �      $�  �  �      LOGICAL,    getDataSource   �      0�      `�  �  �      HANDLE, getDataSourceEvents @�      h�      ��  �  �      CHARACTER,  getDataSourceNames  |�      ��      ܘ  �  �      CHARACTER,  getDataTarget   ��      �      �  �  �      CHARACTER,  getDataTargetEvents ��      $�      X�  �  �      CHARACTER,  getDBAware  8�      d�      ��  � 
       LOGICAL,    getDesignDataObject p�      ��      Й  �        CHARACTER,  getDynamicObject    ��      ܙ      �  �  1      LOGICAL,    getInstanceProperties   �      �      T�  �  B      CHARACTER,  getLogicalObjectName    4�      `�      ��  �  X      CHARACTER,  getLogicalVersion   x�      ��      ؚ  �  m      CHARACTER,  getObjectHidden ��      �      �  �        LOGICAL,    getObjectInitialized    ��       �      X�  �  �      LOGICAL,    getObjectName   8�      d�      ��  �  �      CHARACTER,  getObjectPage   t�      ��      Л  �  �      INTEGER,    getObjectVersion    ��      ܛ      �  �  �      CHARACTER,  getObjectVersionNumber  �      �      T�  �  �      CHARACTER,  getParentDataKey    4�      `�      ��  �  �      CHARACTER,  getPassThroughLinks t�      ��      Ԝ  �  �      CHARACTER,  getPhysicalObjectName   ��      ��      �  �        CHARACTER,  getPhysicalVersion  ��      $�      X�  �  #      CHARACTER,  getPropertyDialog   8�      d�      ��  �  6      CHARACTER,  getQueryObject  x�      ��      ԝ  �  H      LOGICAL,    getRunAttribute ��      ��      �  �  W      CHARACTER,  getSupportedLinks   �      �      P�  �  g      CHARACTER,  getTranslatableProperties   0�      \�      ��  �  y      CHARACTER,  getUIBMode  x�      ��      О  � 
 �      CHARACTER,  getUserProperty ��      ܞ      �  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �      4�      l�  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles L�      ��      ��  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    ��      �      �  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      P�      |�  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   \�      �      �  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      <�      l�  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  L�      ��      ġ  �        CHARACTER,  setChildDataKey ��      С       �  �        LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �      (�      \�  �  "      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  <�      |�      ��  �  5      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    ��      Т      �  �  H      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �      0�      d�  �  a      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   D�      ��      ��  �  u      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ��      ܣ      �  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �      8�      l�  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   L�      ��      Ĥ  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents ��      �      �  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      @�      l�  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject L�      ��      ��  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    ��      �      �  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      8�      p�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalVersion   P�      ��      Ȧ  �        LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      �      �  �  $      LOGICAL,INPUT pcName CHARACTER  setObjectVersion    ��      <�      p�  �  2      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    P�      ��      ̧  �  C      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      ��      (�  �  T      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �      H�      ��  �  h      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  `�      ��      Ԩ  �  ~      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute ��      ��      (�  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �      P�      ��  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   d�      ��      �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ĩ      �      4�  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      T�      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage d�      Ī      �  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   Ъ      �      @�  � 	 �      CHARACTER,INPUT pcName CHARACTER    8�    �	  ��  ��      �       4   �����                 �                      ��                  �	  
                  L��                       �	  ��        �	  (�  ��      �       4   �����                 ��                      ��                  �	  
                  L��                       �	  8�  ��    �	  Ь  L�      �       4   �����                 \�                      ��                  
  	
                  Д�                       
  �         
                                  d     
                    � ߱        �  $  
  ��  ���                           $  
  �  ���                       �                         � ߱        D�    
  T�  Ю      �      4   �����                �                      ��                  
  �
                  ���                       
  d�  �  o   
      ,                                 l�  $   
  @�  ���                       4  @                        � ߱        ��  �   
  T      ��  �   
  �      ��  �   
  <      ��  �   
  �      Я  �    
  $      �  �   "
  �      ��  �   #
        �  �   $
  P       �  �   '
  �      4�  �   )
  8      H�  �   *
  �      \�  �   ,
  0      p�  �   -
  �      ��  �   .
  �      ��  �   /
  d      ��  �   0
  �      ��  �   6
  	      ԰  �   8
  �	      �  �   >
  �	      ��  �   @
  8
      �  �   B
  �
      $�  �   C
  (      8�  �   I
  �      L�  �   J
        `�  �   K
  �      t�  �   L
        ��  �   O
  |      ��  �   P
  �      ��  �   R
  ,      ı  �   S
  h      ر  �   U
  �      �  �   V
         �  �   W
  T      �  �   X
  �      (�  �   Y
  �      <�  �   Z
  H      P�  �   [
  �      d�  �   ]
  �      x�  �   ^
  �      ��  �   _
  8      ��  �   a
  t      ��  �   b
  �      Ȳ  �   c
  �      ܲ  �   d
  (          �   e
  d                      �          t�  \�      ��                  �
  -  ��              Ġ�                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                P                     `                         � ߱        4�  $   ��  ���                           O   +  ��  ��  �               ��          ��  ��    ��                                             ��                            ����                                h      �      L�     T     ��                       ��  P                     �    M  `�  ܵ      �      4   �����                �                      ��                  N  �                  ���                       N  p�   �  �   Q        �  �   R  �      (�  �   S  �      <�  �   T  x      P�  �   U  �      d�  �   V  p      x�  �   W  �      ��  �   X  `      ��  �   Y  �      ��  �   Z  X      ȶ  �   [  �      ܶ  �   \  H      �  �   ]  �          �   ^  @      ܹ    �   �  ��      �      4   �����                ��                      ��                  �  n                  ,y                       �  0�  ��  �   �        Է  �   �  �      �  �   �  �      ��  �   �  t      �  �   �  �      $�  �   �  \      8�  �   �  �      L�  �   �  L      `�  �   �  �      t�  �   �  4       ��  �   �  �       ��  �   �  $!      ��  �   �  �!      ĸ  �   �  "      ظ  �   �  �"      �  �   �  #       �  �   �  �#      �  �   �  $      (�  �   �  �$      <�  �   �  �$      P�  �   �  x%      d�  �   �  �%      x�  �   �  p&      ��  �   �  �&      ��  �   �  h'      ��  �   �  �'      ȹ  �   �  `(          �   �  �(      ��    z  ��  t�      D)      4   ����D)                ��                      ��                  {  ,                  x{                       {  �  ��  �   ~  �)      ��  �      *      ��  �   �  �*      Ժ  �   �  +      �  �   �  �+      ��  �   �  �+      �  �   �  l,      $�  �   �  �,      8�  �   �  -      L�  �   �  X-      `�  �   �  �-      t�  �   �  .      ��  �   �  |.      ��  �   �  �.      ��  �   �  l/      Ļ  �   �  �/      ػ  �   �  T0      �  �   �  �0       �  �   �  L1      �  �   �  �1      (�  �   �  �1      <�  �   �  p2      P�  �   �  �2      d�  �   �   3      x�  �   �  \3      ��  �   �  �3      ��  �   �  4      ��  �   �  P4      ȼ  �   �  �4      ܼ  �   �  �4      �  �   �  5      �  �   �  @5      �  �   �  |5      ,�  �   �  �5      @�  �   �  ,6      T�  �   �  h6      h�  �   �  �6      |�  �   �  �6      ��  �   �  7      ��  �   �  X7      ��  �   �  �7      ̽  �   �  8      �  �   �  |8      ��  �   �  �8      �  �   �  d9      �  �   �  �9      0�  �   �  \:      D�  �   �  �:      X�  �   �  T;      l�  �   �  �;      ��  �   �  L<      ��  �   �  �<      ��  �   �  =      ��  �   �  @=      о  �   �  |=      �  �   �  �=          �   �  ,>      �    :  �  ��      �>      4   �����>  	              ��                      ��             	     ;  �                  t�                       ;  $�  ��  �   =  �>      ȿ  �   >  h?      ܿ  �   ?  �?      �  �   @  X@      �  �   F  �@      �  �   G  hA      ,�  �   H  �A      @�  �   I  PB      T�  �   J  �B      h�  �   K  HC      |�  �   L  �C      ��  �   M  8D      ��  �   N  tD      ��  �   P  �D      ��  �   Q  \E      ��  �   R  �E      ��  �   S  DF      �  �   T  �F      �  �   U  ,G      0�  �   V  �G      D�  �   W  H      X�  �   X  �H      l�  �   Y  I      ��  �   Z  �I      ��  �   [  �I      ��  �   ]  0J      ��  �   ^  �J      ��  �   `  K      ��  �   a  �K      ��  �   b  L          �   c  �L      ��    �  (�  ��      �L      4   �����L  
              ��                      ��             
     �  X                  t��                       �  8�  ��  �   �  M      ��  �   �  �M          �   �  N      ��      �  ��      <N      4   ����<N                ��                      ��                    $                  ���                         �  �      ��  ��      TN      4   ����TN      $    ��  ���                       �N  @         �N              � ߱              !  8�  H�      �N      4   �����N      $  "  t�  ���                       O  @         �N              � ߱        ��  $  ,  ��  ���                       <O     
                    � ߱        ��    e  �  $�      PO      4   ����PO      /   f  P�     `�                          3   ����`O            ��                      3   �����O  ��    o  ��  (�  �  �O      4   �����O                8�                      ��                  p  �                  ���                       p  ��  L�  �   t  �O      ��  $  u  x�  ���                       (P     
                    � ߱        ��  �   v  HP      �  $   x  ��  ���                       pP  @         \P              � ߱        ��  $  {  <�  ���                       �P                         � ߱        �Q     
                R                     \S  @        
 S              � ߱        \�  V   �  h�  ���                        hS                     �S       	       	       �S                         � ߱        ��  $  �  ��  ���                       �T     
                U                     dV  @        
 $V              � ߱        |�  V   �  ��  ���                        pV     
                �V                     <X  @        
 �W              � ߱            V   �  �  ���                                      ��                      ��                  �  �                  h��                       �  ��  PX     
                �X                     Z  @        
 �Y          �Z  @        
 DZ          �Z  @        
 �Z          D[  @        
 [              � ߱            V     $�  ���                        adm-clone-props �  �              �     U     `                          \  �"                     start-super-proc    �  t�  �           �     V                                  �"                     |�    �   �  �      �^      4   �����^      /   �  <�     L�                          3   �����^            l�                      3   ���� _  ��  $  �  ��  ���                        _       
       
           � ߱        ��    �  ��  l�  �  <_      4   ����<_                ��                      ��                  �  �                  �Mu                       �   �  P_       
       
       d_                     x_                         � ߱            $  �  |�  ���                             �  (�  d�      �_      4   �����_  �_       
       
           � ߱            $  �  8�  ���                       ��    �  ��  ��  �  �_      4   �����_      $  �  ��  ���                       �_                         � ߱            �     �_      8`     
                �`                     b  @        
 �a              � ߱        ��  V     (�  ���                        ��  �   I  b      d�    �  ��  ��      Pb      4   ����Pb      /   �  $�     4�                          3   ����`b            T�                      3   �����b  D�    3  ��  ��      �b      4   �����b                �                      ��                  4  7                  <�                       4  ��      g   5  $�         ����                           ��          ��  ��      ��                  6      ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  6  �     (�  �b                      3   �����b  X�     
   H�                      3   �����b         
   x�                      3   �����b    ��                              ��        �                  ����                                        8�              W      ��                      g                               L�  g   9  \�          ��	��                           $�          ��  ��      ��                  9  ;  �              D�                    O   ����    e�          O   ����    R�          O   ����    ��          /  :  P�     `�  �b                      3   �����b            ��                      3   ����c    ��                              ��        �                  ����                                        p�              X      ��                      g                               T�  g   =  d�          ��	��                           ,�          ��  ��      ��                  =  ?  �              �Iu                    O   ����    e�          O   ����    R�          O   ����    ��          /  >  X�     h�  <c                      3   ���� c            ��                      3   ����Dc    ��                              ��        �                  ����                                        x�              Y      ��                      g                               ��    V  p�  ��      `c      4   ����`c                ��                      ��                  W  v                  �Ju                       W  ��  h�  /   X  (�     8�                          3   ����pc            X�                      3   �����c  d�  /  Z  ��     ��  �c                      3   �����c  ��     
   ��                      3   �����c  �        ��                      3   �����c  4�        $�                      3   �����c            T�                      3   ����d  ��    b  ��  ��      8d      4   ����8d      /  h  ��     ��  �d                      3   �����d  ��     
   ��                      3   �����d  ,�        �                      3   �����d  \�        L�                      3   �����d            |�                      3   ����e        n  ��  ��      (e      4   ����(e      /  q  ��     ��  |e                      3   ����\e  $�     
   �                      3   �����e  T�        D�                      3   �����e  ��        t�                      3   �����e            ��                      3   �����e  L�     �  �e                                     �e     
                pf                     �g  @        
 �g              � ߱        ��  V   �  ��  ���                        �g     
                Ph                     �i  @        
 `i              � ߱        �  V     x�  ���                        ��    H  $�  ��      �i      4   �����i                ��                      ��                  I  N                   �]                       I  4�  �  /   J  ��     ��                          3   �����i            �                      3   �����i      /   L  H�     X�                          3   ���� j  ��     
   x�                      3   ���� j  ��        ��                      3   ����(j  ��        ��                      3   ����<j            �                      3   ����Xj  displayObjects  ��  �                      Z      �                               B$                     \�  g   �  ��         �4 �                           d�          4�  �      ��                  �      L�              \-�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��         �j                      3   ����tj    ��                              ��        �                  ����                                        ��              [      ��                      g                               �  g   �  t�          �0��      }                      <�          �  ��      ��                  �      $�              �]                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  h�         �j                      3   �����j    ��                            ����                                        ��              \      x�                      g                               ��    �  0�  ��      �j      4   �����j                ��                      ��                  �                    l�]                       �  @�  (�  /   �  ��     ��                          3   �����j            �                      3   �����j      /     T�     d�  (k                      3   ����k  ��     
   ��                      3   ����0k  ��        ��                      3   ����8k  ��        ��                      3   ����Lk            �                      3   ����lk  �k                     �k                     �k                     8l                         � ߱        l�  $  
  $�  ���                       �l     
                m                     Xn  @        
 n          �n  @        
 pn          o  @        
 �n              � ߱        ��  V     ��  ���                        0o  @         o          Xo  @         Do              � ߱            $     ��  ���                       disable_UI  (�  �                      ]                                    �$  
                    �  �   ���  �                ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  4�  @�      returnFocus ,INPUT hTarget HANDLE   $�  h�  |�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    X�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �  (�      removeAllLinks  ,   �  <�  L�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ,�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  0�  <�      hideObject  ,    �  P�  \�      exitObject  ,   @�  p�  ��      editInstanceProperties  ,   `�  ��  ��      displayLinks    ,   ��  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��   �  ,�      applyEntry  ,INPUT pcField CHARACTER    �  X�  h�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER H�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  $�  ,�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE �  ��  ��      unbindServer    ,INPUT pcMode CHARACTER p�  ��  ��      startServerObject   ,   ��  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  �  0�      restartServerObject ,   �  D�  \�      initializeServerObject  ,   4�  p�  ��      disconnectObject    ,   `�  ��  ��      destroyServerObject ,   ��  ��  ��      bindServer  ,   ��  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  �  ,�      enableObject    ,   �  @�  P�      disableObject   ,   0�  d�  p�      applyLayout ,   T�  ��  ��      viewPage    ,INPUT piPageNum INTEGER    t�  ��  ��      viewObject  ,   ��  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  �  (�      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER �  d�  p�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  T�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  $�  @�      initializeVisualContainer   ,   �  T�  `�      hidePage    ,INPUT piPageNum INTEGER    D�  ��  ��      destroyObject   ,   |�  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  ��  ��      createObjects   ,   ��  �  �      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  ��  ��      changePage  ,   ��  ��  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER ��  �  �      validateFields  ,INPUT-OUTPUT pcNotValidFields CHARACTER    ��  P�  \�      updateTitle ,   @�  p�  ��      updateRecord    ,   `�  ��  ��      updateMode  ,INPUT pcMode CHARACTER ��  ��  ��      showDataMessagesProcedure   ,OUTPUT pcReturn CHARACTER  ��  �  �      resetRecord ,    �  0�  @�      queryPosition   ,INPUT pcState CHARACTER     �  l�  ��      okToContinueProcedure   ,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL   \�  ��  ��      deleteRecord    ,   ��  ��  ��      dataAvailable   ,INPUT pcRelative CHARACTER ��  (�  4�      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  �  d�  t�      confirmDelete   ,INPUT-OUTPUT plAnswer LOGICAL  T�  ��  ��      confirmContinue ,INPUT-OUTPUT plCancel LOGICAL  ��  ��  ��      collectChanges  ,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER ��  D�  P�      viewRecord  ,   4�  d�  t�      valueChanged    ,   T�  ��  ��      updateState ,INPUT pcState CHARACTER    x�  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  ��  �      initializeObject    ,   ��  �  ,�      enableFields    ,   �  @�  P�      displayFields   ,INPUT pcColValues CHARACTER    0�  ��  ��      disableFields   ,INPUT pcFieldType CHARACTER    p�  ��  ��      copyRecord  ,   ��  ��  ��      cancelRecord    ,   ��  �  �      addRecord   ,        � 
"     
 ^%     adecomm/as-utils.w  
"   
   �    }        �
"     
    �     }        �� �  E   %               � 
" 	   
 � %              � �  �         `      $              
�    � �   �      
�             �G                      
�            �     � 
" 	   
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �            7%               
"   
 _�           H    1�   
 _�    � %               o%   o           �      _
"   
 _�           �    1� !   _�    � %               o%   o           � /   _
"   
 _�           0    1� 6  
 _�    � %               o%   o           � A   _
"   
 _�           �    1� Q   _�    � %               o%   o           � _   _
"   
 _�               1� e   _�    � %               o%   o           � t   _
"   
 _�           �    1� �   _� �   � %               o%   o           %               
"   
 � �              1� �   � � �     
"   
 _�           D    1� �   _�    � %               o%   o           � �  � _
"   
 _�           �    1� �   _�    � %               o%   o           � �  N _
"   
 _�           ,    1� �   _� �   � %               o%   o           %               
"   
 _�           �    1� �   _� �   � %               o%   o           %               
"   
 _�           $    1�    _� �   � %               o%   o           %              
"   
 � �          �    1�    � � �     
"   
 _�           �    1� $  
 _� �   � %               o%   o           %               
"   
 _�           X    1� /   _�    � %               o%   o           �      _
"   
 � �          �    1� 7   � � �     
"   
 _�           	    1� G   _�    � %               o%   o           � ]  t _
"   
 � �          |	    1� �  
 � � �     
"   
 _�           �	    1� �   _�    � %               o%   o           � �  � _
"   
 _�           ,
    1� {   _�    � %               o%   o           �      _
"   
 _�           �
    1� �  
 _� �   � %               o%   o           %               
"   
 ��               1� �   �� �   � %               o%   o           %               
"   
 ��           �    1� �   ��    � %               o%   o           �      �
"   
 ��               1� �   ��    � %               o%   o           o%   o           
"   
 ��           �    1� �  
 ��    � %               o%   o           �      �
"   
 ��           �    1� �   �� �  	 � %               o%   o           � �  / �
"   
 � �          p    1�     � � �  	   
"   
 ��           �    1� 2   �� �  	 � o%   o           o%   o           �      �
"   
 � �               1� E   � � �  	   
"   
 ��           \    1� T   �� �  	 � o%   o           o%   o           �      �
"   
 � �          �    1� d   � � �     
"   
 � �              1� r   � � �  	   
"   
 � �          H    1�    � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 ��           �    1� �   �� �   � o%   o           o%   o           %              
"   
 � �          <    1� �   � � �  	   
"   
 � �          x    1� �  
 � � �     
"   
 � �          �    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          ,    1� �   � � �  	   
"   
 � �          h    1�    � � �  	   
"   
 � �          �    1�   	 � � �  	   
"   
 � �          �    1�    � � �  	   
"   
 � �              1� /   � � �  	   
"   
 ��           X    1� F   ��    � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
 �(�  L ( l       �             �� R   � P   �        ,    �@    
� @  , 
�       8    �� [     p�               �L
�    %              � 8      D    � $         � b          
�    � |     
"   
 �� @  , 
�       T    �� 6  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �                1�   
 �    � %               o%   o           �      
"   
 �           t    1� �  
 �    � %               o%   o           o%   o           
"   
 ��           �    1� �   �� �   � %               o%   o           o%   o           
"   
 ��           l    1� �   �� �   � %               o%   o           %               
"   
 ��           �    1� �   �� �   � %               o%   o           %               
"   
 ^�           d    1� �   ^�    � %               o%   o           �      �
"   
 ��           �    1� �   �� �   � %               o%   o           %              
"   
 ��           T    1� �   �� �   � %               o%   o           o%   o           
"   
 ��           �    1� �   ��    � %               o%   o           o%   o           
"   
 ��           L    1� �  	 ��    � %               o%   o           �      �
"   
 ��           �    1� �   ��    � %               o%   o           o%   o           
"   
 ��           <    1�    ��    � %               o%   o           o%   o           
"   
 ��           �    1�    �� �   � %               o%   o           %               
"   
 ��           4    1� *   �� �   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �               1� 6   � �  	 � %               o%   o           �      
"   
 ��           x    1� C   �� �  	 � %               o%   o           �      
"   
 �           �    1� Q   � �   � %               o%   o           %               
"   
 ^�           h    1� _   ^� �  	 � %               o%   o           �      
"   
 ��           �    1� n   �� �  	 � %               o%   o           �      ^
"   
 ��           P    1� |   �� �   � %               o%   o           %               
"   
 ��           �    1� �   �� �  	 � %               o%   o           �      �
"   
 ��           @    1� �   �� �  	 � %               o%   o           �      �
"   
 �           �    1� �   � �  	 � %               o%   o           �      �
"   
 �           (     1� �   � �  	 � %               o%   o           o%   o           
"   
 �           �     1� �   � �  	 � %               o%   o           �      �
"   
 ^�           !    1� �   ^� �  	 � %               o%   o           �      
"   
 ��           �!    1� �  	 �� �   � %               o%   o           %               
"   
 ��           "    1� �   �� �   � %               o%   o           %               
"   
 ��           �"    1� �   �� �   � %               o%   o           o%   o           
"   
 ��            #    1�    �� �   � %               o%   o           o%   o           
"   
 �           |#    1�    � �   � %               o%   o           %               
"   
 ��           �#    1� #   �� �   � %               o%   o           %               
"   
 �           t$    1� 4   � �   � %               o%   o           %               
"   
 ^�           �$    1� I   ^� U   � %               o%   o           %       
       
"   
 ^�           l%    1� ]   ^� U   � %               o%   o           o%   o           
"   
 ��           �%    1� i   �� U   � %               o%   o           %              
"   
 ��           d&    1� u   �� U   � %               o%   o           o%   o           
"   
 ��           �&    1� �   �� U   � %               o%   o           %              
"   
 ��           \'    1� �   �� U   � %               o%   o           o%   o           
"   
 ��           �'    1� �   �� U   � %               o%   o           %              
"   
 ��           T(    1� �   �� U   � %               o%   o           o%   o           
"   
 ^�           �(    1� �   ^� �  	 � %               o%   o           �      �P �L 
�H T   %              �     }        �GG %              
"   
 ��           �)    1� �   �� �   � %               o%   o           %               
"   
 ��           *    1� �   �� �   � %               o%   o           o%   o           
"   
 ��           �*    1� �   ��    � %               o%   o           �      �
"   
 ��           +    1� �   ��    � %               o%   o           � �  - �
"   
 �           x+    1� )   �    � %               o%   o           �      �
"   
 ��           �+    1� @   ��    � %               o%   o           � ]   
"   
 � �          `,    1� {   � � �     
"   
 ��           �,    1� �   ��    � %               o%   o           �      
"   
 � �          -    1� �  
 � � �     
"   
 � �          L-    1� �   � � �     
"   
 ��           �-    1� �   �� �  	 � %               o%   o           �      �
"   
 ��           �-    1� �   ��    � %               o%   o           �      �
"   
 ��           p.    1� �   �� �   � %               o%   o           o%   o           
"   
 ��           �.    1� �   ��    � %               o%   o           � �  ! �
"   
 ��           `/    1�    ��    � %               o%   o           �      �
"   
 ^�           �/    1�    ^�    � %               o%   o           � ,   �
"   
 ^�           H0    1� ;  	 ^� �   � %               o%   o           o%   o           
"   
 ��           �0    1� E   �� �   � %               o%   o           %               
"   
 � �          @1    1� Q   � � �     
"   
 ��           |1    1� _   ��    � %               o%   o           � s   
"   
 ��           �1    1� �   �� �  	 � %               o%   o           �      �
"   
 ��           d2    1� �   �� �  	 � %               o%   o           �      �
"   
 � �          �2    1� �   � � �     
"   
 � �          3    1� �   � � �  	   
"   
 ^�           P3    1� �   ^� �   � o%   o           o%   o           %               
"   
 � �          �3    1� �   � � �     
"   
 � �          4    1� �   � � �  	   
"   
 � �          D4    1�     � � �  	   
"   
 � �          �4    1�    � � �  	   
"   
 � �          �4    1� $   � � �  	   
"   
 � �          �4    1� 5   � � �  	   
"   
 � �          45    1� F   � � �     
"   
 ��           p5    1� W   ��    � %               o%   o           � n  4 �
"   
 � �          �5    1� �   � � �     
"   
 � �           6    1� �   � � �     
"   
 � �          \6    1� �   � � �     
"   
 � �          �6    1� �   � � �  	   
"   
 � �          �6    1� �   � � �  	   
"   
 � �          7    1� �   � � �  	   
"   
 � �          L7    1�    � � �     
"   
 ��           �7    1�    �� �  	 � %               o%   o           �      �
"   
 ��           �7    1�     �� �  	 � %               o%   o           �      �
"   
 ��           p8    1� ,   �� �  	 � %               o%   o           �      �
"   
 ��           �8    1� A   �� �  	 � %               o%   o           �      �
"   
 �           X9    1� V   � �   � %               o%   o           %               
"   
 �           �9    1� d   � �   � %               o%   o           o%   o           
"   
 ��           P:    1� v   �� �   � %               o%   o           %               
"   
 ��           �:    1� �   �� �   � %               o%   o           %               
"   
 ��           H;    1� �   �� �   � %               o%   o           o%   o           
"   
 ��           �;    1� �   �� �   � %               o%   o           %               
"   
 � �          @<    1� �   � � �  	   
"   
 ��           |<    1� �   �� �   � %               o%   o           %              
"   
 � �          �<    1� �   � � �  	   
"   
 � �          4=    1� �   � � �  	   
"   
 � �          p=    1� �  
 � � �  	   
"   
 ��           �=    1�     �� �  	 � %               o%   o           � V   �
"   
 �            >    1�    � �  	 � %               o%   o           �      �P �L 
�H T   %              �     }        �GG %              
"   
 ��           �>    1� #   ��    � %               o%   o           �      �
"   
 ��           \?    1� 1   �� �   � %               o%   o           %               
"   
 ��           �?    1� >   ��    � %               o%   o           �      �
"   
 ��     ,      L@    1� N   ��    � %               o%   o           �   � �     � ^   ��    	 �
"   
 ��           �@    1� `   �� �   � %               o%   o           o%   o           
"   
 ��           \A    1� i   ��    � %               o%   o           �      �
"   
 �           �A    1� w   �    � %               o%   o           �      �
"   
 �           DB    1� �   � �  	 � %               o%   o           o%   o           
"   
 �           �B    1� �   �    � %               o%   o           o%   o           
"   
 ^�           <C    1� �   ^�    � %               o%   o           �      �
"   
 ��           �C    1� �   �� �   � %               o%   o           %               
"   
 � �          ,D    1� �   � � �     
"   
 ��           hD    1� �   ��    � %               o%   o           � �  ~ �
"   
 ��           �D    1� q    ��    � %               o%   o           �      �
"   
 ��           PE    1� �    ��    � %               o%   o           � �    �
"   
 �           �E    1� �    � �  	 � %               o%   o           � �    �
"   
 ��           8F    1� �    �� �  	 � %               o%   o           � �    
"   
 ��           �F    1� �   	 ��    � %               o%   o           � �    �
"   
 ��            G    1� �   
 �� �  	 � %               o%   o           � �    �
"   
 ��           �G    1� !   �� �   � %               o%   o           o%   o           
"   
 ��           H    1� !   ��    � %               o%   o           � "!   �
"   
 ��           �H    1� 4!   ��    � %               o%   o           �      �
"   
 ��           �H    1� =!  
 �� �   � %               o%   o           o%   o           
"   
 � �          tI    1� H!   � � �     
"   
 ��           �I    1� V!   ��    � %               o%   o           � j!  ] 
"   
 ��           $J    1� �!   ��    � %               o%   o           �      �
"   
 ��           �J    1� �!   ��    � %               o%   o           � �!   �
"   
 ��           K    1� �!   �� �   � %               o%   o           %               
"   
 �           �K    1� �   �    � %               o%   o           �      �
"   
 �           �K    1� �!   �    � %               o%   o           o%   o           
"   
 � �          xL    1� "   � � �  	   P �L 
�H T   %              �     }        �GG %              
"   
 �           M    1� "   � �   � %               o%   o           %               
"   
 ��           �M    1� 0"  	 �� �   � %               o%   o           %               
"   
 � �           N    1� :"   � �          
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              
�             �G "    � %     start-super-proc w� %     adm2/smart.p ��P �L 
�H T   %              �     }        �GG %              
"   
   �       �O    6� R     
"   
   
�        P    8
"   
   �        <P    ��     }        �G 4              
"   
 ߱G %              G %              %� � �   EnabledObjFldsToDisable,ModifyFields,DataSourceNames,UpdateTargetNames,LogicalObjectName,LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �Q    �� R   � P   �        �Q    �@    
� @  , 
�       �Q    �� [   �p�               �L
�    %              � 8       R    � $         � b          
�    � |   �
"   
 �p� @  , 
�       S    �� �   �p�               �L"    , �   � u"   �� w"   � �     }        �A      |    "      � u"   �%              (<   \ (    |    �     }        �A� y"   �A"  	  �    "    �"  	  �  < "    �"  	  �(    |    �     }        �A� y"   �A"  	  �
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �T    �� R   � P   �        �T    �@    
� @  , 
�       �T    �� [   �p�               �L
�    %              � 8      U    � $         � b          
�    � |   �
"   
 �p� @  , 
�       V    ��   
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        �V    �� R   � P   �        �V    �@    
� @  , 
�       �V    �� [   �p�               �L
�    %              � 8      �V    � $         � b   �     
�    � |   � 
"   
 �p� @  , 
�       �W    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        �X    �� R   � P   �        �X    �@    
� @  , 
�       �X    �� [     p�               �L
�    %              � 8      �X    � $         � b          
�    � |     
"   
 �p� @  , 
�       �Y    �� 6  
 �p�               �L%     SmartDataViewer 
"   
   p� @  , 
�       8Z    �� Q     p�               �L%      FRAME   
"   
  p� @  , 
�       �Z    �� T    p�               �L%               
"   
  p� @  , 
�       �Z    �� 2    p�               �L(        �        �        �        �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 �    �        �[    �� R   �
"   
   � 8      $\    � $         � b          
�    � |   �
"   
   �        |\    �
"   
   �       �\    /
"   
   
"   
   �       �\    6� R     
"   
   
�        �\    8
"   
   �        ]    �
"   
   �       4]    �
"   
   p�    � �"   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �]    �A"    �A
"   
   
�        D^    �@ � 
"   
 �"      �       }        �
"   
 � %              %                "    � %     start-super-proc v� %     adm2/appserver.p ���    � ##     
�    �     }        �%               %      Server  - �     }        �    "  
  �      � %                   "    �      � %      NONE    p�,  8         $     "    �        � =#   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �`    �� R   � P   �        �`    �@    
� @  , 
�       �`    �� [   �p�               �L
�    %              � 8      �`    � $         � b          
�    � |   �
"   
 �p� @  , 
�       �a    �� �   �p�               �L"    , p�,  8         $     "  
  �        � K#   �
�     "    � %     start-super-proc v� %     adm2/visual.p �� 
"    
 � %     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP ��%     processAction   
�    %     CTRL-PAGE-DOWN  "    � %     start-super-proc u� %     adm2/containr.p %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    � �#   �
�    � �#   � A    �    � �#     
�    � �#   � %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    � �#   � 
�    � �#   %     modifyListProperty  
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        @f    �� R   � P   �        Lf    �@    
� @  , 
�       Xf    �� [   �p�               �L
�    %              � 8      df    � $         � b   �     
�    � |   � 
"   
 �p� @  , 
�       tg    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �         h    �� R   � P   �        ,h    �@    
� @  , 
�       8h    �� [   �p�               �L
�    %              � 8      Dh    � $         � b   �     
�    � |   �
"   
 �p� @  , 
�       Ti    �� V   �p�               �L%               "    � %     start-super-proc t� %     adm2/datavis.p %     modifyListProperty  
�    %      ADD     %     SupportedLinks %     Toolbar-Target %     valueChanged    
�    %     valueChanged    
�     "    � %     start-super-proc s� %     adm2/viewer.p �%     modifyListProperty  
�    
�    %      Add     %     DataSourceEvents w%     buildDataRequest w�   � �   �� ^     � �$  & ��   � �     � ^   �� �$  & ��@    �    � �   �� �$   �     � �   �"    �� �   � �@    �    � �     � �$         � �   �"    � � �     
�H T   %              �     }        �GG %              
"   
 � 
"   
 �
"   
 � 
"   
 � (�  L ( l       �        �l    �� R   � P   �        �l    �@    
� @  , 
�       �l    �� [   � p�               �L
�    %              � 8      �l    � $         � b   �      
�    � |     
"   
 �p� @  , 
�       n    �� >   �p�               �L"    , 
"   
   p� @  , 
�       dn    �� i     p�               �L"    , 
"   
  p� @  , 
�       �n    �� =!  
  p�               �L%               �             I%               �             �%              �     }        �
�                    �           �   l       ��                   '  �               ܢ�                    O   ����    e�          O   ����    R�          O   ����    ��        $    �   ���                       �[     
                    � ߱                (  �      �[      4   �����[                �                      ��                    &                  �}]                         8  �  �    0\              �  `      �\      4   �����\                p                      ��                    %                  @~]                         �  �  o         ,                                 �  �     �\      �  �     �\      $  $    �  ���                        ]     
                    � ߱        8  �      ]      L  �     @]      `  �   !  `]          $   $  �  ���                       �]  @         |]              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 K  �  �               �]                    O   ����    e�          O   ����    R�          O   ����    ��      �"                      �          �  $  ]    ���                       �]     
                    � ߱                  �  �                      ��                   ^  `                  T�]                     ^  4      4   ����^      $  _  �  ���                       P^     
                    � ߱        �    a  4  D      d^      4   ����d^      /  b  p                               3   ����x^  �  �   }  �^          O   �  ��  ��  �^                               , �                          
                               �      ��                            ����                                                        �   l       ��                  �  �  �               �]                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  4  @  �               ,��                    O   ����    e�          O   ����    R�          O   ����    ��      �      >  �� �                       ?  �         lo      4   ����lo      �   ?  �o    ��                              ��        �                  ����                               �    d d     �   ���  �  � �                                               �      �                                                                  d     D                                                                 P   �d Kd                                                           �$  G   
 X �d �d                                                              �     g     �       P   �� Zd                                                           �$  G   
 X �� /d                                                       !      �  (   g     �        D                                                                    TXS appSrvUtils RowObject CodAlm Descripcion CodCia ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST s-codcia F-Main x(3) C�digo de almac�n X(40) Descripci�n de almac�n C:\newsie\on_in_co\util\xxx\vtablewi.w should only be RUN PERSISTENT. GETTARGETPROCEDURE GETOBJECTTYPE GETSHOWPOPUP SETSHOWPOPUP GETCREATEHANDLES GETDATAMODIFIED GETDISPLAYEDFIELDS GETDISPLAYEDTABLES GETENABLEDFIELDS GETENABLEDHANDLES GETFIELDHANDLES GETFIELDSENABLED GETGROUPASSIGNSOURCE GETGROUPASSIGNSOURCEEVENTS GETGROUPASSIGNTARGET GETGROUPASSIGNTARGETEVENTS GETNEWRECORD GETOBJECTPARENT GETRECORDSTATE GETROWIDENT GETTABLEIOSOURCE GETTABLEIOSOURCEEVENTS GETUPDATETARGET GETUPDATETARGETNAMES GETWINDOWTITLEFIELD OKTOCONTINUE SETCONTAINERMODE SETDATAMODIFIED SETDISPLAYEDFIELDS SETENABLEDFIELDS SETGROUPASSIGNSOURCE SETGROUPASSIGNSOURCEEVENTS SETGROUPASSIGNTARGET SETGROUPASSIGNTARGETEVENTS SETLOGICALOBJECTNAME SETOBJECTPARENT SETSAVESOURCE SETTABLEIOSOURCE SETTABLEIOSOURCEEVENTS SETUPDATETARGET SETUPDATETARGETNAMES SETWINDOWTITLEFIELD SHOWDATAMESSAGES DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETWAITFOROBJECT SETWINDOWTITLEVIEWER SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALVERSION SETOBJECTNAME SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataViewer ContainerType FRAME PropertyDialog adm2/support/viewerd.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties EnabledObjFldsToDisable,ModifyFields,DataSourceNames,UpdateTargetNames,LogicalObjectName,LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName CreateHandles DataModified DisplayedFields DisplayedTables   Editable EnabledFields EnabledHandles EnabledObjFldsToDisable EnabledWhenNew FieldHandles FieldsEnabled GroupAssignSource GroupAssignSourceEvents addRecord,copyRecord,updateRecord,resetRecord,undoRecord,cancelRecord,enableFields,disableFields,collectChanges,validateFields GroupAssignTarget GroupAssignTargetEvents updateState,LinkState InternalDisplayFromSource (Large) ModifyFields (All) NewRecord No ObjectMode View PrintPreviewActive RecordState NoRecordAvailable RowIdent SaveSource TableIOSource TableIOSourceEvents addRecord,updateRecord,copyRecord,deleteRecord,resetRecord,undoChange,cancelRecord,updateMode ToolbarSource ToolbarSourceEvents toolbar UndoNew UpdateTargetNames WindowTitleField KeepChildPositions ShowPopup FieldWidgetIDs ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target adm2/datavis.p ADD Toolbar-Target DISPLAYOBJECTS cViewCols cEnabled iCol iEntries cEntry adm2/viewer.p RowObject.CodAlm RowObject.Descripcion ,RowObject. DISABLE_UI default Almac�n Descripci�n �  8"  �  �*      3 �    ��      0         pcFieldType     ��      T         pcColValues     ��      x         pcValue     ��      �         pcState �   ��      �         pcChanges       ��      �         pcChanges       ��               plCancel        ��      $        plAnswer        ��      H        plCancel        ��      l        pcRelative  �  ��      �        pcAction        ��      �        pcAction        ��      �        pcState     ��      �        pcReturn        ��              pcMode      ��      <        pcState     ��      \        pcNotValidFields    �  ��      �        pcProp      ��      �        pcProp      ��      �        plCancel    �  ��      �        pcProcName    ��             
 pcProcName  @  ��      4        pcProcName      ��      X       
 pcProcName      ��      |        piPageNum       ��      �        piPageNum       ��      �        pcPageList      ��      �        pcProc    ��              pcLinkName      ��      ,        pcLinkName  \  ��      P       
 phTarget        ��      t        phTarget        ��      �        piPageNum       ��      �        pcValue     ��      �        piPageNum       ��               pcAction        ��      $       
 phAppService        ��      L        pcMode  x  ��      l       
 phSource    �  ��      �        phSource        ��      �       
 phSource    �  ��      �        pcText     ��      �        pcText      ��              pcText  D  ��      8       
 phObject    h  ��      \       
 phObject        ��      �        phObject        ��      �        pcField     ��      �        pcCursor    �  ��      �       
 phCaller      ��              phCaller    <  ��      0        phCaller        ��      T        phCaller    �  ��      x        pcMod   �  ��      �        pcMod       ��      �       
 pcMod   �  ��      �       
 phSource      ��      �        phSource        ��              
 phSource    L  ��      D        pdRow       ��      d        pdRow       ��      �       
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             	     cType       T	     T   �          D	                  getObjectType     +  -  �	        t	  
   hReposBuffer    �	        �	  
   hPropTable  �	        �	  
   hBuffer           �	  
   hTable  	  
     U   `	          
                  adm-clone-props                         !  $  %  &  '            t
  
   hProc             �
        pcProcName  �	  �
  	   V   `
  |
      �
                  start-super-proc    ]  ^  _  `  a  b  }  �  �  �
  8     W                                   6    l     X                                   :  ;  <  �     Y                                   >  ?  t  �     Z               �                  displayObjects  �  �        [                                   �  �  T     \                                   �  $  �     ]               �                  disable_UI  >  ?  @  X  0  $    
 ,                                �  �     RowObject                     $         CodAlm  Descripcion CodCia  L          @  
   appSrvUtils l        `     s-codcia    �        �  
   gshAstraAppserver   �        �  
   gshSessionManager   �        �  
   gshRIManager            �  
   gshSecurityManager  0          
   gshProfileManager   \  	 	     D  
   gshRepositoryManager    �  
 
     p  
   gshTranslationManager   �        �  
   gshWebManager   �        �     gscSessionId    �        �     gsdSessionObj             
   gshFinManager   <        ,  
   gshGenManager   `        P  
   gshAgnManager   �        t     gsdTempUniqueID �        �     gsdUserObj  �        �     gsdRenderTypeObj    �        �     gsdSessionScopeObj           
   ghProp  0       $  
   ghADMProps  T       D  
   ghADMPropsBuf   |       h     glADMLoadFromRepos  �       �     glADMOk �       �  
   ghContainer �       �     cObjectName �    	   �     iStart      
        cAppService 4       (     cASDivision `       H     cServerOperatingMode    |       t     cFields �       �     cViewCols   �       �     cEnabled    �       �     iCol    �       �     iEntries                  cEntry        X  $  RowObject            D   =  >  @  A  �	  �	  �	  �	  �	  
  
  	
  
  
  
  
  
  
  
  
  
  
  
  
   
  "
  #
  $
  '
  )
  *
  ,
  -
  .
  /
  0
  6
  8
  >
  @
  B
  C
  I
  J
  K
  L
  O
  P
  R
  S
  U
  V
  W
  X
  Y
  Z
  [
  ]
  ^
  _
  a
  b
  c
  d
  e
  �
  M  N  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  n  z  {  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ,  :  ;  =  >  ?  @  F  G  H  I  J  K  L  M  N  P  Q  R  S  T  U  V  W  X  Y  Z  [  ]  ^  `  a  b  c  �  �  �  �  �  �  X          !  "  $  ,  e  f  o  p  t  u  v  x  {  �  �  �  �  �  �    �  �  �  �  �  �  �  �  �  �  �  �      I  �  �  3  4  5  7  9  =  V  W  X  Z  b  h  n  q  v  �  �    H  I  J  L  N  �  �  �  �  �       
          :%  C:\Progress\OpenEdge\src\adm2\viewer.i   �  �Q 2 %C:\Progress\OpenEdge\src\adm2\custom\viewercustom.i  �  } & C:\Progress\OpenEdge\src\adm2\datavis.i  4  � 1 %C:\Progress\OpenEdge\src\adm2\custom\dataviscustom.i h  f! ' C:\Progress\OpenEdge\src\adm2\containr.i �  � 0 %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �  �� ( C:\Progress\OpenEdge\src\adm2\visual.i      # / %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  T  �< ) C:\Progress\OpenEdge\src\adm2\appserver.i    �  �� . %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �  I� * C:\Progress\OpenEdge\src\adm2\smart.i      Ds - C:\Progress\OpenEdge\gui\fn  D  tw , %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   l  Q. + C:\Progress\OpenEdge\gui\set �  �/  C:\Progress\OpenEdge\src\adm2\viewprop.i �  �� $ %C:\Progress\OpenEdge\src\adm2\custom\viewpropcustom.i      ۃ % %C:\Progress\OpenEdge\src\adm2\custom\viewprtocustom.i    L  ��  C:\Progress\OpenEdge\src\adm2\dvisprop.i �  B� " %C:\Progress\OpenEdge\src\adm2\custom\dvispropcustom.i    �  �� # %C:\Progress\OpenEdge\src\adm2\custom\dvisprtocustom.i      ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i L  ��   %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �  P ! %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �  F>  C:\Progress\OpenEdge\src\adm2\visprop.i    �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i <  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i |  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    4  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i x  �j  C:\Progress\OpenEdge\gui\get �  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i      ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i \  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i     �  C:\Progress\OpenEdge\src\adm2\appsprto.i H  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   |  �X  C:\Progress\OpenEdge\src\adm2\visprto.i  �  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i <   ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    p   �7 
 C:\Progress\OpenEdge\src\adm2\dvisprto.i �   0 	 %C:\Progress\OpenEdge\src\adm2\custom\datavisdefscustom.i �   ��  C:\Progress\OpenEdge\src\adm2\viewprto.i 0!  gf  %C:\Progress\OpenEdge\src\adm2\custom\viewerdefscustom.i  d!  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �!  �  .\util\xxx\dtables.i �!  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i    "  �|    C:\newsie\on_in_co\util\xxx\vtablewi.w       �          l"  �   �     |"     �  2   �"  �   |     �"     Z  +   �"  �   W     �"     5  +   �"  �   4     �"       +   �"  \   �     �"  o   �  &   #     R  1   #  U   8  &   ,#  �   1  '   <#       +   L#  �   
  '   \#     �  +   l#  �   �  '   |#     �  0   �#  �   q  '   �#     o  -   �#  �   h  '   �#     f  -   �#  �   e  '   �#     c  -   �#  r   G  '   �#  n   /  (   $     �  /   $  P   �  (   ,$  �   �  )   <$     X  .   L$  �   S  )   \$     1  +   l$  �   0  )   |$       +   �$  �     )   �$     �  +   �$  g   �  )   �$     �     �$  O   �  )   �$  �   #  *   �$     !  -   �$  �   �  *   %     �  ,   %  �   �  *   ,%     l  +   <%  �   k  *   L%     I  +   \%  �   H  *   l%     &  +   |%  �   %  *   �%       +   �%  �   �  *   �%     �  +   �%  �   �  *   �%     �  +   �%  }   �  *   �%     }  +   �%       *   &     �  )   &     d  (   ,&     �  '   <&     �  &   L&     a     \&  u   X     l&  O   J  $   |&     9  %   �&     �  $   �&  h   �     �&  �   �     �&  O   �  "   �&     �  #   �&     h  "   �&  {   5     �&  �   ,     '  O         '       !   ,'     �      <'  �   w     L'  �   n     \'  O   `     l'     O     |'          �'  �   �     �'  x   �     �'  M   �     �'     �     �'     b     �'  a   K     �'  �  *     �'          (  �  �
     (  O   �
     ,(     �
     <(     k
     L(  �   �	     \(     g     l(     �     |(  x   �     �(     �     �(     &     �(     "     �(          �(     �     �(  Q   �     �(     �     �(     S     )     ?     )     %     ,)  f   �     <)     �     L)  "   U     \)     A     l)           |)  Z   �     �)     �     �)     �     �)     �     �)     j     �)  X   G     �)     �  
   �)      Y     �)     E  	   *     &     *  ]        ,*     �     <*     �     L*     �     \*     q     l*     T     |*  0   �       �*     M      �*     *       �*     &      �*     !       �*           