	��V�CY ?    �              �                                 �� 3F000143utf-8 MAIN C:\newsie\on_in_co\aplic\vcustomer.w,, PROCEDURE disable_UI,, PROCEDURE displayObjects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER PROCEDURE validateFields,,INPUT-OUTPUT pcNotValidFields CHARACTER PROCEDURE updateTitle,, PROCEDURE updateRecord,, PROCEDURE updateMode,,INPUT pcMode CHARACTER PROCEDURE showDataMessagesProcedure,,OUTPUT pcReturn CHARACTER PROCEDURE resetRecord,, PROCEDURE queryPosition,,INPUT pcState CHARACTER PROCEDURE okToContinueProcedure,,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE deleteRecord,, PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE confirmDelete,,INPUT-OUTPUT plAnswer LOGICAL PROCEDURE confirmContinue,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE collectChanges,,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER PROCEDURE viewRecord,, PROCEDURE valueChanged,, PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE initializeObject,, PROCEDURE enableFields,, PROCEDURE displayFields,,INPUT pcColValues CHARACTER PROCEDURE disableFields,,INPUT pcFieldType CHARACTER PROCEDURE copyRecord,, PROCEDURE cancelRecord,, PROCEDURE addRecord,, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION showDataMessages,CHARACTER, FUNCTION setWindowTitleField,LOGICAL,INPUT cWindowTitleField CHARACTER FUNCTION setUpdateTargetNames,LOGICAL,INPUT pcTargetNames CHARACTER FUNCTION setUpdateTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setTableIOSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setTableIOSource,LOGICAL,INPUT phObject HANDLE FUNCTION setSaveSource,LOGICAL,INPUT plSave LOGICAL FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setLogicalObjectName,LOGICAL,INPUT pcLogicalObjectName CHARACTER FUNCTION setGroupAssignTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setGroupAssignSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignSource,LOGICAL,INPUT phObject HANDLE FUNCTION setEnabledFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDisplayedFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDataModified,LOGICAL,INPUT plModified LOGICAL FUNCTION setContainerMode,LOGICAL,INPUT pcContainerMode CHARACTER FUNCTION okToContinue,LOGICAL,INPUT pcAction CHARACTER FUNCTION getWindowTitleField,CHARACTER, FUNCTION getUpdateTargetNames,CHARACTER, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getTableIOSourceEvents,CHARACTER, FUNCTION getTableIOSource,HANDLE, FUNCTION getRowIdent,CHARACTER, FUNCTION getRecordState,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getNewRecord,CHARACTER, FUNCTION getGroupAssignTargetEvents,CHARACTER, FUNCTION getGroupAssignTarget,CHARACTER, FUNCTION getGroupAssignSourceEvents,CHARACTER, FUNCTION getGroupAssignSource,HANDLE, FUNCTION getFieldsEnabled,LOGICAL, FUNCTION getFieldHandles,CHARACTER, FUNCTION getEnabledHandles,CHARACTER, FUNCTION getEnabledFields,CHARACTER, FUNCTION getDisplayedTables,CHARACTER, FUNCTION getDisplayedFields,CHARACTER, FUNCTION getDataModified,LOGICAL, FUNCTION getCreateHandles,CHARACTER, FUNCTION setShowPopup,LOGICAL,INPUT plShowPopup LOGICAL FUNCTION getShowPopup,LOGICAL, FUNCTION getObjectType,character, FUNCTION getTargetProcedure,HANDLE, FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER        `              L             �� `  ��              �o              ,    +   �� �  U   �� `  V   �� �   Z   �� t  ]           d� �  ?  � y'  iSO8859-1                                                                           �    �                                      �                  ��                    �     �   
o   ث           ��  �   (      4                                                       PROGRESS                         4           
    
                    �              �                                                                                                     
  T         �          X    4   H     �      t                       �          $      �                sports2017                       PROGRESS                         d     g        g                         ���G            g  {K                              �  �                        �        REPNAMEREGIONSALESREPMONTHQUOTA                                        �  �      \  
    
                  H               �                                                                                          �          
  �  �        
    
                  �  �             x                                                                                          �          
  8  �      �  
    
                  �  h             $                                                                                          �          
  �  �      `  
    
                  L               �                                                                                          �          
  �  �        
    
                  �  �             |                                                                                          �          
  <        �  
    
                  �  l  	           (                                                                                                    
  �  "      d  
    
                  P    
           �                                                                                          "          
  �  8        
    
                  �  �             �                                                                                          8          
  @	  F      �                         �  p	             ,	                                                                                          F            �	  S      h	                        T	  
             �	                                                                                          S            �
  a      
  
    
                   
  �
             �
                                                                                          a          
  D  o      �
  
    
                  �
  t             0                                                                                          o          
  �  }      l  
    
                  X                �                                                                                          }          
  �  �                                �             �                                                                                          �            H  �      �                        �  x             4                                                                                          �            �  �      p                        \  $             �                                                                                          �                �                                               �                                                                                          �                          ,�                                               0�            `  H X            
             
             
                                         
                                                                                                                                                                        H   X   h   x   �   �   �   �   �   �   �   �       (  8  H      H   X   h   x   �   �   �   �   �   �   �   �      (  8  H                                                                                                                                     	                  
                                                                                                                                                                                                   h  p  x  �              �             �  �  �  �              �             �  �  �                                $  ,  4              8             P  \  d  p              t             �  �  �  �              �             �  �  �  �              �             �             P  $  �             �  �  �  �      �  �  �               (  0  <      h  @  |             �  �  �  �              �             �                                  ,  4  <  D              H             `  h  p  x              |             �  �  �  �              �             �  �  �        T    |             �  �  �  �              �             �  �  �  �                                                                        Address x(35)   Address     Please enter an address.    Address2    x(35)   Address2        Please enter an address.    Balance ->,>>>,>>9.99   Balance 0   Please enter a balance. City    x(25)   City        Please enter a city.    Comments    x(80)   Comments        Please enter comments.  Contact x(30)   Contact     Please enter a contact. Country x(20)   Country USA Please enter a country. CreditLimit ->,>>>,>>9  Credit Limit    1500    Credit Limit must be >= 0 and <= 9,999,999  CreditLimit >= 0 AND CreditLimit <= 9999999 .   Please enter a Credit Limit.    CustNum >>>>9   Cust Num    0   Customer number must be greater than zero   custnum > 0 .   Please enter a customer number. Discount    >>9%    Discount    0   Discount must be greater or equal to 0  Discount >= 0 .     Please enter a percentage from 0 to 100.    EmailAddress    x(50)   Email       Please enter an full Internet Email Address.    Fax x(20)   Fax     Please enter a fax number.  Name    x(30)   Name        Please enter a name.    Phone   x(20)   Phone       Please enter a phone number PostalCode  x(10)   Postal Code     Please enter the appropriate Postal Code.   SalesRep    x(4)    Sales Rep       The Sales Rep's name you've entered must exist in the Salesrep table.   CAN-FIND ( Salesrep OF RowObject ) .    Please Enter a Sales Rep.   State   x(20)   State       Please enter standard state abbreviation.   Terms   x(20)   Terms   Net30   Please enter terms  �  %�  ���������      USA�          Net30�    0'                �     i     	       "   +   3   8   A   I   Q   ]   e   n   {      �   �   �   �   �     ��                                               )          ����                            �    ;S    undefined                                                               �       H�  �   l   X�                        �����               ��g                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       (     S          assignFocusedWidget         �      �     �       LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    �       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          �       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    �       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4          LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �          LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    1      LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  >      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  S      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 l      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    w      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 �      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    �      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    �      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    �      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	           LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	          CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER        A   -       �
   ��         �
        0                                   d                               |            �          �            �
   �
    �    `  D  �      �       4   �����                 �                      ��                  `  d                  |_�                       `  T    	  a                                          3   �����       O   c  ��  ��  �   addRecord                               �  �      ��                      �              ��<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                �  �      ��                      �              (2�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyRecord                              �  �      ��                      �              �2�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableFields                               �  �      ��                      �              P3�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            displayFields                               �  �      ��                                    (��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                              ��                            ����                            enableFields                                        ��                  !  "  4              �=%                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                $        ��                  $  %  <              D>%                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            toolbar                                      ��                  '  )  8              ��-                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  P           ��                            ����                            updateState                             H  0      ��                  +  -  `              X@                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  x           ��                            ����                            valueChanged                                t  \      ��                  /  0  �              �r�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewRecord                              t  \      ��                  2  3  �              (s�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            getTargetProcedure  
      �          :      HANDLE, getObjectType   �             P    M      CHARACTER,  getShowPopup    0      \      �    [      LOGICAL,    setShowPopup    l      �      �    h      LOGICAL,INPUT plShowPopup LOGICAL   addRecord                               |  d      ��                  �  �  �              ȡ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                �  h      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            collectChanges                              �  l      ��                  �  �  �              t]                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            confirmContinue                             �  �      ��                  �  �  �              �3                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            confirmDelete                                 �      ��                  �  �                43                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  4           ��                            ����                            confirmExit                             ,        ��                  �  �  D              �H�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  \           ��                            ����                            copyRecord                              T   <       ��                  �  �  l               ��U                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            dataAvailable                               X!  @!      ��                  �  �  p!              ȍU                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �!           ��                            ����                            deleteRecord                                �"  l"      ��                  �  �  �"              `O3                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �#  t#      ��                  �  �  �#              �,�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            okToContinueProcedure                               �$  �$      ��                  �  �  �$              �-�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �$             �$               ��                  �$           ��                            ����                            queryPosition                               �%  �%      ��                  �  �  &              �F                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  &           ��                            ����                            resetRecord                             '  �&      ��                  �  �  ,'              �e�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            showDataMessagesProcedure                               $(  (      ��                  �  �  <(               f�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  T(           ��                            ����                            updateMode                              L)  4)      ��                  �  �  d)              tf�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  |)           ��                            ����                            updateRecord                                x*  `*      ��                  �  �  �*              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             x+  `+      ��                  �  �  �+              �&�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �+           ��                            ����                            updateTitle                             �,  �,      ��                  �     �,              pO                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            validateFields                              �-  �-      ��                      �-              (Җ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �-           ��                            ����                            getCreateHandles    �      <.      p.    u      CHARACTER,  getDataModified P.      |.      �.    �      LOGICAL,    getDisplayedFields  �.      �.      �.    �      CHARACTER,  getDisplayedTables  �.      �.      ,/    �      CHARACTER,  getEnabledFields    /      8/      l/     �      CHARACTER,  getEnabledHandles   L/      x/      �/  !  �      CHARACTER,  getFieldHandles �/      �/      �/  "  �      CHARACTER,  getFieldsEnabled    �/      �/      (0  #  �      LOGICAL,    getGroupAssignSource    0      40      l0  $         HANDLE, getGroupAssignSourceEvents  L0      t0      �0  %        CHARACTER,  getGroupAssignTarget    �0      �0      �0  &  0      CHARACTER,  getGroupAssignTargetEvents  �0       1      <1  '  E      CHARACTER,  getNewRecord    1      H1      x1  (  `      CHARACTER,  getObjectParent X1      �1      �1  )  m      HANDLE, getRecordState  �1      �1      �1  *  }      CHARACTER,  getRowIdent �1      �1      $2  +  �      CHARACTER,  getTableIOSource    2      02      d2  ,  �      HANDLE, getTableIOSourceEvents  D2      l2      �2  -  �      CHARACTER,  getUpdateTarget �2      �2      �2  .  �      CHARACTER,  getUpdateTargetNames    �2      �2      $3  /  �      CHARACTER,  getWindowTitleField 3      03      d3  0  �      CHARACTER,  okToContinue    D3      p3      �3  1  �      LOGICAL,INPUT pcAction CHARACTER    setContainerMode    �3      �3      �3  2        LOGICAL,INPUT pcContainerMode CHARACTER setDataModified �3       4      P4  3        LOGICAL,INPUT plModified LOGICAL    setDisplayedFields  04      t4      �4  4  '      LOGICAL,INPUT pcFieldList CHARACTER setEnabledFields    �4      �4       5  5  :      LOGICAL,INPUT pcFieldList CHARACTER setGroupAssignSource    �4      $5      \5  6  K      LOGICAL,INPUT phObject HANDLE   setGroupAssignSourceEvents  <5      |5      �5  7  `      LOGICAL,INPUT pcEvents CHARACTER    setGroupAssignTarget    �5      �5      6  8  {      LOGICAL,INPUT pcObject CHARACTER    setGroupAssignTargetEvents  �5      86      t6  9  �      LOGICAL,INPUT pcEvents CHARACTER    setLogicalObjectName    T6      �6      �6  :  �      LOGICAL,INPUT pcLogicalObjectName CHARACTER setObjectParent �6      �6      ,7  ;  �      LOGICAL,INPUT phParent HANDLE   setSaveSource   7      L7      |7  <  �      LOGICAL,INPUT plSave LOGICAL    setTableIOSource    \7      �7      �7  =  �      LOGICAL,INPUT phObject HANDLE   setTableIOSourceEvents  �7      �7      (8  >  �      LOGICAL,INPUT pcEvents CHARACTER    setUpdateTarget 8      L8      |8  ?        LOGICAL,INPUT pcObject CHARACTER    setUpdateTargetNames    \8      �8      �8  @        LOGICAL,INPUT pcTargetNames CHARACTER   setWindowTitleField �8       9      49  A  +      LOGICAL,INPUT cWindowTitleField CHARACTER   showDataMessages    9      `9      �9  B  ?      CHARACTER,  assignPageProperty                              8:   :      ��                    	  P:              Dq�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �:             h:               ��                  �:           ��                            ����                            changePage                              �;  p;      ��                      �;              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �<  p<      ��                      �<              �H�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �<           ��                            ����                            constructObject                             �=  �=      ��                      �=              |U?                    O   ����    e�          O   ����    R�          O   ����    ��            ��   >             �=               �� 
  @>             >  
             ��   h>             4>               �� 
                 \>  
         ��                            ����                            createObjects                               X?  @?      ��                      p?              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              X@  @@      ��                      p@              L�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �@           ��                            ����                            destroyObject                               �A  lA      ��                     !  �A              (�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �B  lB      ��                  #  %  �B              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �B           ��                            ����                            initializeObject                                �C  �C      ��                  '  (  �C              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               �D  �D      ��                  *  +  �D              �7                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               �E  �E      ��                  -  /  �E              �7                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �E           ��                            ����                            notifyPage                              �F  �F      ��                  1  3  G              �	7                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  G           ��                            ����                            passThrough                             H  �G      ��                  5  8  ,H              4f�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   xH             DH               ��                  lH           ��                            ����                            removePageNTarget                               lI  TI      ��                  :  =  �I              (��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �I             �I  
             ��                  �I           ��                            ����                            selectPage                              �J  �J      ��                  ?  A  �J              ,DE                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �J           ��                            ����                            toolbar                             �K  �K      ��                  C  E  �K              ��;                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  L           ��                            ����                            viewObject                              M  �L      ��                  G  H   M              ��;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                N  �M      ��                  J  L   N              ,`�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8N           ��                            ����                            disablePagesInFolder    t9      �N      �N  C  P      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �N      O      8O  D  e      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  O      dO      �O  E  y      HANDLE, getCallerWindow xO      �O      �O  F  �      HANDLE, getContainerMode    �O      �O      P  G  �      CHARACTER,  getContainerTarget  �O      P      LP  H  �      CHARACTER,  getContainerTargetEvents    ,P      XP      �P  I  �      CHARACTER,  getCurrentPage  tP      �P      �P  J  �      INTEGER,    getDisabledAddModeTabs  �P      �P      Q  K  �      CHARACTER,  getDynamicSDOProcedure  �P       Q      XQ  L  �      CHARACTER,  getFilterSource 8Q      dQ      �Q  M        HANDLE, getMultiInstanceActivated   tQ      �Q      �Q  N  &      LOGICAL,    getMultiInstanceSupported   �Q      �Q       R  O  @      LOGICAL,    getNavigationSource  R      ,R      `R  P  Z      CHARACTER,  getNavigationSourceEvents   @R      lR      �R  Q  n      CHARACTER,  getNavigationTarget �R      �R      �R  R  �      HANDLE, getOutMessageTarget �R      �R      $S  S  �      HANDLE, getPageNTarget  S      ,S      \S  T  �      CHARACTER,  getPageSource   <S      hS      �S  U  �      HANDLE, getPrimarySdoTarget xS      �S      �S  V  �      HANDLE, getReEnableDataLinks    �S      �S      T  W  �      CHARACTER,  getRunDOOptions �S       T      PT  X  �      CHARACTER,  getRunMultiple  0T      \T      �T  Y  	      LOGICAL,    getSavedContainerMode   lT      �T      �T  Z  	      CHARACTER,  getSdoForeignFields �T      �T      U  [  +	      CHARACTER,  getTopOnly  �T      U      HU  \ 
 ?	      LOGICAL,    getUpdateSource (U      TU      �U  ]  J	      CHARACTER,  getWaitForObject    dU      �U      �U  ^  Z	      HANDLE, getWindowTitleViewer    �U      �U      V  _  k	      HANDLE, getStatusArea   �U      V      <V  `  �	      LOGICAL,    pageNTargets    V      HV      xV  a  �	      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject XV      �V      �V  b  �	      LOGICAL,INPUT h HANDLE  setCallerProcedure  �V      �V      ,W  c  �	      LOGICAL,INPUT h HANDLE  setCallerWindow W      DW      tW  d  �	      LOGICAL,INPUT h HANDLE  setContainerTarget  TW      �W      �W  e  �	      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �W      �W      X  f  �	      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �W      0X      hX  g  �	      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  HX      �X      �X  h  
      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �X      �X       Y  i  
      LOGICAL,INPUT phObject HANDLE   setInMessageTarget   Y      @Y      tY  j  .
      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   TY      �Y      �Y  k  A
      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �Y       Z      <Z  l  [
      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource Z      lZ      �Z  m  u
      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �Z      �Z       [  n  �
      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �Z      $[      X[  o  �
      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget 8[      x[      �[  p  �
      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �[      �[      �[  q  �
      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �[       \      P\  r  �
      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget 0\      p\      �\  s  �
      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �\      �\      ]  t  �
      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �\      0]      `]  u        LOGICAL,INPUT phObject HANDLE   setRunDOOptions @]      �]      �]  v  !      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �]      �]      ^  w  1      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �]      (^      `^  x  @      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields @^      �^      �^  y  V      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �^      �^      _  z 
 j      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �^      8_      h_  {  u      LOGICAL,INPUT pcSource CHARACTER    setWaitForObject    H_      �_      �_  |  �      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �_      �_      `  }  �      LOGICAL,INPUT phViewer HANDLE   setStatusArea   �_      8`      h`  ~  �      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             a  a      ��                  �  �  4a              h�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                                b  b      ��                  �  �  8b              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                $c  c      ��                  �  �  <c              ��y                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                ,d  d      ��                  �  �  Dd              ��y                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               0e  e      ��                  �  �  He              ��y                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `e           ��                            ����                            getAllFieldHandles  H`      �e      �e    �      CHARACTER,  getAllFieldNames    �e      f      <f  �  �      CHARACTER,  getCol  f      Hf      pf  �  �      DECIMAL,    getDefaultLayout    Pf      |f      �f  �  �      CHARACTER,  getDisableOnInit    �f      �f      �f  �  �      LOGICAL,    getEnabledObjFlds   �f      �f      0g  �        CHARACTER,  getEnabledObjHdls   g      <g      pg  �        CHARACTER,  getHeight   Pg      |g      �g  � 	 *      DECIMAL,    getHideOnInit   �g      �g      �g  �  4      LOGICAL,    getLayoutOptions    �g      �g      $h  �  B      CHARACTER,  getLayoutVariable   h      0h      dh  �  S      CHARACTER,  getObjectEnabled    Dh      ph      �h  �  e      LOGICAL,    getObjectLayout �h      �h      �h  �  v      CHARACTER,  getRow  �h      �h      i  �  �      DECIMAL,    getWidth    �h       i      Li  �  �      DECIMAL,    getResizeHorizontal ,i      Xi      �i  �  �      LOGICAL,    getResizeVertical   li      �i      �i  �  �      LOGICAL,    setAllFieldHandles  �i      �i      j  �  �      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �i      ,j      `j  �  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    @j      �j      �j  �  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �j      �j      k  �  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �j      ,k      \k  �        LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    <k      |k      �k  �        LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �k      �k      l  �  !      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �k      (l      \l  �  1      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   <l      �l      �l  �  E      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �l      �l      m  �  W      LOGICAL,    getObjectSecured    �l      $m      Xm  �  k      LOGICAL,    createUiEvents  8m      dm      �m  �  |      LOGICAL,    bindServer                              0n  n      ��                  �  �  Hn              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               4o  o      ��                  �  �  Lo               ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             <p  $p      ��                  �  �  Tp              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                Dq  ,q      ��                  �  �  \q              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              Pr  8r      ��                  �  �  hr              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             Xs  @s      ��                  �  �  ps              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             \t  Dt      ��                  �  �  tt              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �t  
         ��                            ����                            startServerObject                               �u  tu      ��                  �  �  �u              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �v  xv      ��                  �  �  �v              h�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �v           ��                            ����                            getAppService   tm      (w      Xw  �  �      CHARACTER,  getASBound  8w      dw      �w  � 
 �      LOGICAL,    getAsDivision   pw      �w      �w  �  �      CHARACTER,  getASHandle �w      �w      x  �  �      HANDLE, getASHasStarted �w      x      <x  �  �      LOGICAL,    getASInfo   x      Hx      tx  � 	 �      CHARACTER,  getASInitializeOnRun    Tx      �x      �x  �  �      LOGICAL,    getASUsePrompt  �x      �x      �x  �  �      LOGICAL,    getServerFileName   �x       y      4y  �  �      CHARACTER,  getServerOperatingMode  y      @y      xy  �        CHARACTER,  runServerProcedure  Xy      �y      �y  �  %      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �y      �y      ,z  �  8      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   z      Tz      �z  �  F      LOGICAL,INPUT pcDivision CHARACTER  setASHandle dz      �z      �z  �  T      LOGICAL,INPUT phASHandle HANDLE setASInfo   �z      �z       {  � 	 `      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun     {      @{      x{  �  j      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  X{      �{      �{  �        LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �{      �{       |  �  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode   |      D|      ||  �  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             8}   }      ��                  �  �  P}              |$s                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �}             h}  
             ��   �}             �}               �� 
                 �}  
         ��                            ����                            addMessage                              �~  �~      ��                  �  �  �~              4��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                �~               ��   <                            ��                  0           ��                            ����                            adjustTabOrder                              ,�  �      ��                  �  �  D�              ��{                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  ��             \�  
             �� 
  ��             ��  
             ��                  ��           ��                            ����                            applyEntry                              ��  ��      ��                  �  �  ��              @D                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  ԁ           ��                            ����                            changeCursor                                Ђ  ��      ��                  �  �  �              �p�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                   �           ��                            ����                            createControls                              ��  �      ��                  �  �  �              4u�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                                �  �      ��                  �  �  �              �w�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �  �      ��                  �  �  �              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �  ��      ��                  �  �  (�              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �  ��      ��                  �  �  (�              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �  ��      ��                  �  �  (�              P�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �   �      ��                  �  �  0�              (V�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                               �  �      ��                  �  �  8�              W�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  ��             P�  
             ��   ��             x�               ��   ԋ             ��               ��                  ȋ           ��                            ����                            modifyUserLinks                             Č  ��      ��                  �  �  ܌              ��<                    O   ����    e�          O   ����    R�          O   ����    ��            ��   (�             �               ��   P�             �               �� 
                 D�  
         ��                            ����                            removeAllLinks                              @�  (�      ��                  �  �  X�              $�o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              @�  (�      ��                  �  �  X�              ��o                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  ��             p�  
             ��   ̏             ��               �� 
                 ��  
         ��                            ����                            repositionObject                                ��  ��      ��                  �  �  ؐ              ��L                    O   ����    e�          O   ����    R�          O   ����    ��            ��   $�             �               ��                  �           ��                            ����                            returnFocus                             �  ��      ��                  �  �  (�              ,��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 @�  
         ��                            ����                            showMessageProcedure                                D�  ,�      ��                  �  �  \�              p��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   ��             t�               ��                  ��           ��                            ����                            toggleData                              ��  |�      ��                  �  �  ��              <x@                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  Ĕ           ��                            ����                            viewObject                              ��  ��      ��                  �  �  ԕ              l-:                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  \|      ,�      X�  � 
       LOGICAL,    assignLinkProperty  8�      d�      ��  �        LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   x�      �       �  �  #      CHARACTER,  getChildDataKey  �      ,�      \�  �  1      CHARACTER,  getContainerHandle  <�      h�      ��  �  A      HANDLE, getContainerHidden  |�      ��      ؗ  �  T      LOGICAL,    getContainerSource  ��      �      �  �  g      HANDLE, getContainerSourceEvents    ��       �      \�  �  z      CHARACTER,  getContainerType    <�      h�      ��  �  �      CHARACTER,  getDataLinksEnabled |�      ��      ܘ  �  �      LOGICAL,    getDataSource   ��      �      �  �  �      HANDLE, getDataSourceEvents ��       �      T�  �  �      CHARACTER,  getDataSourceNames  4�      `�      ��  �  �      CHARACTER,  getDataTarget   t�      ��      Й  �  �      CHARACTER,  getDataTargetEvents ��      ܙ      �  �  �      CHARACTER,  getDBAware  �      �      H�  � 
       LOGICAL,    getDesignDataObject (�      T�      ��  �        CHARACTER,  getDynamicObject    h�      ��      Ț  �  .      LOGICAL,    getInstanceProperties   ��      Ԛ      �  �  ?      CHARACTER,  getLogicalObjectName    �      �      P�  �  U      CHARACTER,  getLogicalVersion   0�      \�      ��  �  j      CHARACTER,  getObjectHidden p�      ��      ̛  �  |      LOGICAL,    getObjectInitialized    ��      ؛      �  �  �      LOGICAL,    getObjectName   �      �      L�  �  �      CHARACTER,  getObjectPage   ,�      X�      ��  �  �      INTEGER,    getObjectVersion    h�      ��      Ȝ  �  �      CHARACTER,  getObjectVersionNumber  ��      Ԝ      �  �  �      CHARACTER,  getParentDataKey    �      �      L�  �  �      CHARACTER,  getPassThroughLinks ,�      X�      ��  �  �      CHARACTER,  getPhysicalObjectName   l�      ��      Н  �  
      CHARACTER,  getPhysicalVersion  ��      ܝ      �  �         CHARACTER,  getPropertyDialog   �      �      P�  �  3      CHARACTER,  getQueryObject  0�      \�      ��  �  E      LOGICAL,    getRunAttribute l�      ��      Ȟ  �  T      CHARACTER,  getSupportedLinks   ��      Ԟ      �  �  d      CHARACTER,  getTranslatableProperties   �      �      P�  �  v      CHARACTER,  getUIBMode  0�      \�      ��  � 
 �      CHARACTER,  getUserProperty h�      ��      ğ  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    ��      �      $�  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles �      L�      x�  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    X�      ��      ̠  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      �      4�  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      ��      С  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ��      $�  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  �      L�      |�  �         CHARACTER,  setChildDataKey \�      ��      ��  �        LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  ��      �      �  �        LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ��      4�      h�  �  2      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    H�      ��      ģ  �  E      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled ��      �      �  �  ^      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ��      D�      t�  �  r      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents T�      ��      Ȥ  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      �      $�  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   �      L�      |�  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents \�      ��      ԥ  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      ��      $�  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject �      D�      x�  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    X�      ��      Ԧ  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      �      (�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalVersion   �      L�      ��  �        LOGICAL,INPUT cVersion CHARACTER    setObjectName   `�      ��      ԧ  �  !      LOGICAL,INPUT pcName CHARACTER  setObjectVersion    ��      ��      (�  �  /      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �      P�      ��  �  @      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks d�      ��      �  �  Q      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   ��       �      8�  �  e      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �      X�      ��  �  {      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute l�      ��      �  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   ��      �      <�  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   �      `�      ��  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  |�      ��      �  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ̪      �      <�  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage �      |�      ��  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   ��      ̫      ��  � 	 �      CHARACTER,INPUT pcName CHARACTER    �    
  8�  ��      �       4   �����                 Ĭ                      ��                  
  2
                  D�:                       
  H�        
  �  \�      �       4   �����                 l�                      ��                  
  1
                  ��:                       
  �  l�    
  ��  �      �       4   �����                 �                      ��                  *
  ,
                  L�:                       *
  ��         +
                                  �     
                    � ߱        ��  $  .
  @�  ���                           $  0
  Į  ���                       �                         � ߱        ��    6
  �  ��      �      4   �����                ��                      ��                  7
  �
                  lD�                       7
  �  ̯  o   :
      ,                                 $�  $   ;
  ��  ���                       \  @         H              � ߱        8�  �   <
  |      L�  �   =
  �      `�  �   ?
  d      t�  �   A
  �      ��  �   C
  L      ��  �   E
  �      ��  �   F
  <      İ  �   G
  x      ذ  �   J
  �      �  �   L
  `       �  �   M
  �      �  �   O
  X      (�  �   P
  �      <�  �   Q
        P�  �   R
  �      d�  �   S
   	      x�  �   Y
  <	      ��  �   [
  �	      ��  �   a
  �	      ��  �   c
  `
      ȱ  �   e
  �
      ܱ  �   f
  P      �  �   l
  �      �  �   m
  @      �  �   n
  �      ,�  �   o
  0      @�  �   r
  �      T�  �   s
  �      h�  �   u
  T      |�  �   v
  �      ��  �   x
        ��  �   y
  @      ��  �   z
  |      ̲  �   {
  �      �  �   |
  �      ��  �   }
  p      �  �   ~
  �      �  �   �
  �      0�  �   �
  $      D�  �   �
  `      X�  �   �
  �      l�  �   �
  �      ��  �   �
        ��  �   �
  P          �   �
  �                      ��          ,�  �      ��                  "  P  D�               G�                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                x                     �                         � ߱        �  $ 6  \�  ���                           O   N  ��  ��  �               X�          H�  P�    8�                                             ��                            ����                                       ��      �     T     `�                       \�  M                     ��    p  �  ��      �      4   �����                ��                      ��                  q  �                  (�d                       q  (�  ��  �   t  4      ̶  �   u  �      �  �   v  $      ��  �   w  �      �  �   x        �  �   y  �      0�  �   z        D�  �   {  �      X�  �   |        l�  �   }  �      ��  �   ~  �      ��  �     p      ��  �   �  �          �   �  h      ��      ط  T�      �      4   �����                d�                      ��                    �                  �d                         �  x�  �     8      ��  �     �      ��  �            ��  �     �      ȸ  �   	        ܸ  �   
  �      �  �            �  �     t      �  �     �      ,�  �     \       @�  �     �       T�  �     L!      h�  �     �!      |�  �     <"      ��  �     �"      ��  �     4#      ��  �     �#      ̹  �     ,$      �  �     �$      ��  �     $%      �  �     �%      �  �     &      0�  �     �&      D�  �     '      X�  �     �'      l�  �     (      ��  �     �(          �      )      ��    �  ��  ,�      l)      4   ����l)                <�                      ��                  �  O                  �0                       �  ��  P�  �   �  �)      d�  �   �  H*      x�  �   �  �*      ��  �   �  8+      ��  �   �  �+      ��  �   �   ,      Ȼ  �   �  �,      ܻ  �   �  �,      �  �   �  D-      �  �   �  �-      �  �   �  �-      ,�  �   �  0.      @�  �   �  �.      T�  �   �   /      h�  �   �  �/      |�  �   �  0      ��  �   �  |0      ��  �   �  �0      ��  �   �  t1      ̼  �   �  �1      �  �   �  $2      ��  �   �  �2      �  �   �  3      �  �   �  H3      0�  �   �  �3      D�  �   �   4      X�  �   �  <4      l�  �   �  x4      ��  �   �  �4      ��  �   �  �4      ��  �   �  ,5      ��  �   �  h5      н  �   �  �5      �  �   �  6      ��  �   �  T6      �  �   �  �6       �  �   �  �6      4�  �   �  7      H�  �   �  D7      \�  �   �  �7      p�  �   �  �7      ��  �   �  08      ��  �   �  �8      ��  �   �  9      ��  �   �  �9      Ծ  �   �  :      �  �   �  �:      ��  �   �   ;      �  �   �  |;      $�  �   �  �;      8�  �   �  t<      L�  �   �  �<      `�  �   �  ,=      t�  �   �  h=      ��  �   �  �=      ��  �   �  �=          �   �  T>      ��    ]  ̿  H�      �>      4   �����>  	              X�                      ��             	     ^  �                  l�                       ^  ܿ  l�  �   `  ?      ��  �   a  �?      ��  �   b  @      ��  �   c  �@      ��  �   i  A      ��  �   j  �A      ��  �   k  B      ��  �   l  xB      �  �   m  �B       �  �   n  pC      4�  �   o  �C      H�  �   p  `D      \�  �   q  �D      p�  �   s  E      ��  �   t  �E      ��  �   u  �E      ��  �   v  lF      ��  �   w  �F      ��  �   x  TG      ��  �   y  �G      ��  �   z  DH      �  �   {  �H      $�  �   |  ,I      8�  �   }  �I      L�  �   ~  �I      `�  �   �  XJ      t�  �   �  �J      ��  �   �  @K      ��  �   �  �K      ��  �   �  0L          �   �  �L      ��      ��  \�      �L      4   �����L  
              l�                      ��             
       {                  ��                         ��  ��  �     <M      ��  �     �M          �     4N      X�    =  ��  @�      dN      4   ����dN                P�                      ��                  >  G                   �                       >  ��  ��    @  l�  |�      |N      4   ����|N      $  A  ��  ���                       �N  @         �N              � ߱              D  ��   �      �N      4   �����N      $  E  ,�  ���                       4O  @          O              � ߱        ��  $  O  ��  ���                       dO     
                    � ߱        H�    �  ��  ��      xO      4   ����xO      /   �  �     �                          3   �����O            8�                      3   �����O  ��    �  d�  ��  ��  �O      4   �����O                ��                      ��                  �                    ��6                       �  t�  �  �   �  $P      \�  $  �  0�  ���                       PP     
                    � ߱        p�  �   �  pP      ��  $   �  ��  ���                       �P  @         �P              � ߱        ��  $  �  ��  ���                       �P                         � ߱        �Q     
                4R                     �S  @        
 DS              � ߱        �  V   �   �  ���                        �S                     �S       	       	        T                         � ߱        ��  $  �  ��  ���                       �T     
                <U                     �V  @        
 LV              � ߱        4�  V   �  @�  ���                        �V     
                W                     dX  @        
 $X              � ߱            V   �  ��  ���                                      ��                      ��                    �                  ��6                         `�  xX     
                �X                     DZ  @        
 Z          �Z  @        
 lZ          [  @        
 �Z          l[  @        
 ,[              � ߱            V   .  ��  ���                        adm-clone-props ��  ��              �     U     `                          \  �$                     start-super-proc    ��  ,�  �           �     V                                  �$                     4�    �  ��  ��      �^      4   �����^      /   �  ��     �                          3   ����_            $�                      3   ����(_  ��  $  �  `�  ���                       H_       
       
           � ߱        H�    �  ��  $�  ��  d_      4   ����d_                ��                      ��                  �  �                  �t�                       �  ��  x_       
       
       �_                     �_                         � ߱            $  �  4�  ���                             �  ��  �      �_      4   �����_  �_       
       
           � ߱            $     ��  ���                       D�      d�  t�  ��  �_      4   �����_      $    ��  ���                       `                         � ߱            �   %   `      ``     
                �`                     ,b  @        
 �a              � ߱        p�  V   9  ��  ���                        ��  �   l  8b      �    �  ��  ��      xb      4   ����xb      /   �  ��     ��                          3   �����b            �                      3   �����b  ��    V  8�  ��      �b      4   �����b                ��                      ��                  W  Z                  �x�                       W  H�      g   X  ��         ����                           ��          t�  \�      ��                  Y      ��              (y�                    O   ����    e�          O   ����    R�          O   ����    ��          /  Y  ��     ��  �b                      3   �����b  �     
    �                      3   �����b         
   0�                      3   ���� c    ��                              ��        )                  ����                                        ��              W      @�                      g                               �  g   \  �          ��	��                           ��          ��  ��      ��                  \  ^  ��              �{�                    O   ����    e�          O   ����    R�          O   ����    ��          /  ]  �     �  $c                      3   ����c            8�                      3   ����,c    ��                              ��        )                  ����                                        (�              X      H�                      g                               �  g   `  �          ��	��                           ��          ��  ��      ��                  `  b  ��              (R�                    O   ����    e�          O   ����    R�          O   ����    ��          /  a  �      �  dc                      3   ����Hc            @�                      3   ����lc    ��                              ��        )                  ����                                        0�              Y      P�                      g                               l�    y  (�  ��      �c      4   �����c                ��                      ��                  z  �                  �R�                       z  8�   �  /   {  ��     ��                          3   �����c            �                      3   �����c  �  /  }  L�     \�  �c                      3   �����c  ��     
   |�                      3   �����c  ��        ��                      3   ����d  ��        ��                      3   ����d            �                      3   ����<d  D�    �  8�  H�      `d      4   ����`d      /  �  t�     ��  �d                      3   �����d  ��     
   ��                      3   �����d  ��        ��                      3   �����d  �        �                      3   ����e            4�                      3   ����0e        �  `�  p�      Pe      4   ����Pe      /  �  ��     ��  �e                      3   �����e  ��     
   ��                      3   �����e  �        ��                      3   �����e  <�        ,�                      3   �����e            \�                      3   �����e  �     �  f                                     f     
                �f                     �g  @        
 �g              � ߱        ��  V     ��  ���                        �g     
                xh                     �i  @        
 �i              � ߱        ��  V   :  0�  ���                        <�    k  ��  X�      �i      4   �����i                h�                      ��                  l  q                  ,x                       l  ��  ��  /   m  ��     ��                          3   �����i            ��                      3   ����j      /   o   �     �                          3   ����(j  @�     
   0�                      3   ����Hj  p�        `�                      3   ����Pj  ��        ��                      3   ����dj            ��                      3   �����j  displayObjects  @�  ��                      Z      �                               ?&                     �  g     T�         �4��                           �          ��  ��      ��                        �              �L                    O   ����    e�          O   ����    R�          O   ����    ��          /    H�         �j                      3   �����j    ��                              ��        )                  ����                                        h�              [      X�                      g                               ��  g     ,�          �0p�      }                      ��          ��  ��      ��                        ��              L                    O   ����    e�          O   ����    R�          O   ����    ��          /     �         �j                      3   �����j    ��                            ����                                        @�              \      0�                      g                               \�       ��  d�      �j      4   �����j                t�                      ��                  !  (                  �L                       !  ��  ��  /   "  ��     ��                          3   �����j            ��                      3   ����k      /  #  �     �  Pk                      3   ����0k  L�     
   <�                      3   ����Xk  |�        l�                      3   ����`k  ��        ��                      3   ����tk            ��                      3   �����k  �k                     �k                     l                     `l                         � ߱        $�  $  -  ��  ���                       �l     
                0m                     �n  @        
 @n          �n  @        
 �n          0o  @        
 �n              � ߱        ��  V   =  ��  ���                        Xo  @         Do          �o  @         lo              � ߱            $   1  P�  ���                       disable_UI  ��  ��                      ]                                    %'  
                    �  �   ���  �                8   ����       8   ����       x�  ��      toggleData  ,INPUT plEnabled LOGICAL    h�  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  �  �      returnFocus ,INPUT hTarget HANDLE   ��  @�  T�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    0�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  ��   �      removeAllLinks  ,   ��  �  $�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE �  |�  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    l�  �  �      hideObject  ,   ��  (�  4�      exitObject  ,   �  H�  `�      editInstanceProperties  ,   8�  t�  ��      displayLinks    ,   d�  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  ��  �      applyEntry  ,INPUT pcField CHARACTER    ��  0�  @�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER  �  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  ��  �      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  X�  h�      unbindServer    ,INPUT pcMode CHARACTER H�  ��  ��      startServerObject   ,   ��  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  ��  �      restartServerObject ,   ��  �  4�      initializeServerObject  ,   �  H�  \�      disconnectObject    ,   8�  p�  ��      destroyServerObject ,   `�  ��  ��      bindServer  ,   ��  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  ��  �      enableObject    ,   ��  �  (�      disableObject   ,   �  <�  H�      applyLayout ,   ,�  \�  h�      viewPage    ,INPUT piPageNum INTEGER    L�  ��  ��      viewObject  ,   ��  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  ��   �      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  <�  H�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ,�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  ��  �      initializeVisualContainer   ,   ��  ,�  8�      hidePage    ,INPUT piPageNum INTEGER    �  d�  t�      destroyObject   ,   T�  ��  ��      deletePage  ,INPUT piPageNum INTEGER    x�  ��  ��      createObjects   ,   ��  ��  ��      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  h�  t�      changePage  ,   X�  ��  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER x�  ��  ��      validateFields  ,INPUT-OUTPUT pcNotValidFields CHARACTER    ��  (�  4�      updateTitle ,   �  H�  X�      updateRecord    ,   8�  l�  x�      updateMode  ,INPUT pcMode CHARACTER \�  ��  ��      showDataMessagesProcedure   ,OUTPUT pcReturn CHARACTER  ��  ��  ��      resetRecord ,   ��  �  �      queryPosition   ,INPUT pcState CHARACTER    ��  D�  \�      okToContinueProcedure   ,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL   4�  ��  ��      deleteRecord    ,   ��  ��  ��      dataAvailable   ,INPUT pcRelative CHARACTER ��   �  �      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  <�  L�      confirmDelete   ,INPUT-OUTPUT plAnswer LOGICAL  ,�  |�  ��      confirmContinue ,INPUT-OUTPUT plCancel LOGICAL  l�  ��  ��      collectChanges  ,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER ��  �  (�      viewRecord  ,   �  <�  L�      valueChanged    ,   ,�  `�  l�      updateState ,INPUT pcState CHARACTER    P�  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  ��  ��      initializeObject    ,   ��  ��  �      enableFields    ,   ��  �  (�      displayFields   ,INPUT pcColValues CHARACTER    �  X�  h�      disableFields   ,INPUT pcFieldType CHARACTER    H�  ��  ��      copyRecord  ,   ��  ��  ��      cancelRecord    ,   ��  ��  ��      addRecord   ,        � 
"     
 u%     adecomm/as-utils.w  
"   
   �    }        �
"     
   #     Sales Rep the&    &     �     }        �� �  C   %               � 
" 	   
 � %              � �  �         `      $              
�    � �   �      
�             �G                      
�            � �   � 
" 	   
 :
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        <    7%               
"   
 ��           p    1�   
 ��    � %               o%   o           �     �
"   
 ��           �    1�    ��    � %               o%   o           � ,   �
"   
 ��           X    1� 3  
 ��    � %               o%   o           � >   �
"   
 ��           �    1� N   ��    � %               o%   o           � \   �
"   
 ��           @    1� b   ��    � %               o%   o           � q   �
"   
 ��           �    1� �   �� �   � %               o%   o           %               
"   
 � �          0    1� �   � � �     
"   
 ��           l    1� �   ��    � %               o%   o           � �  � �
"   
 ��           �    1� �   ��    � %               o%   o           � �  N �
"   
 ��           T    1� �   �� �   � %               o%   o           %               
"   
 ��           �    1� �   �� �   � %               o%   o           %               
"   
 ��           L    1�    �� �   � %               o%   o           %              
"   
 � �          �    1�    � � �     
"   
 ��               1� !  
 �� �   � %               o%   o           %               
"   
 ��           �    1� ,   ��    � %               o%   o           �     �
"   
 � �          �    1� 4   � � �     
"   
 ��           0	    1� D   ��    � %               o%   o           � Z  t �
"   
 � �          �	    1� �  
 � � �     
"   
 ��           �	    1� �   ��    � %               o%   o           � �  � �
"   
 ��           T
    1� x   ��    � %               o%   o           �     �
"   
 ��           �
    1� �  
 �� �   � %               o%   o           %               
"   
 )�           D    1� �   )� �   � %               o%   o           %               
"   
 :�           �    1� �   :�    � %               o%   o           �     )
"   
 :�           4    1� �   :�    � %               o%   o           o%   o           
"   
 ��           �    1� �  
 ��    � %               o%   o           �     w
"   
 :�           $    1� �   :� �  	 � %               o%   o           � �  / �
"   
 � �          �    1�    � � �  	   
"   
 w�           �    1� /   w� �  	 � o%   o           o%   o           �     w
"   
 � �          H    1� B   � � �  	   
"   
 w�           �    1� Q   w� �  	 � o%   o           o%   o           �     w
"   
 � �          �    1� a   � � �     
"   
 � �          4    1� o   � � �  	   
"   
 � �          p    1� |   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 *�           �    1� �   *� �   � o%   o           o%   o           %              
"   
 � �          d    1� �   � � �  	   
"   
 � �          �    1� �  
 � � �     
"   
 � �          �    1� �   � � �  	   
"   
 � �              1� �   � � �  	   
"   
 � �          T    1� �   � � �  	   
"   
 � �          �    1�     � � �  	   
"   
 � �          �    1�   	 � � �  	   
"   
 � �              1�    � � �  	   
"   
 � �          D    1� ,   � � �  	   
"   
 :�           �    1� C   :�    � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 ]
"   
   
"   
 �(�  L ( l       �        H    �� O   � P   �        T    �@    
� @  , 
�       `    �� X     p�               �L
�    %              � 8      l    � $         � _          
�    � y     
"   
 �� @  , 
�       |    �� 3  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ]�           (    1� |  
 ]�    � %               o%   o           �     ]
"   
 ]�           �    1� �  
 ]�    � %               o%   o           o%   o           
"   
 H�               1� �   H� �   � %               o%   o           o%   o           
"   
 :�           �    1� �   :� �   � %               o%   o           %               
"   
 )�               1� �   )� �   � %               o%   o           %               
"   
 u�           �    1� �   u�    � %               o%   o           �     )
"   
 *�                1� �   *� �   � %               o%   o           %              
"   
 *�           |    1� �   *� �   � %               o%   o           o%   o           
"   
 ��           �    1� �   ��    � %               o%   o           o%   o           
"   
 H�           t    1� �  	 H�    � %               o%   o           �     w
"   
 H�           �    1� �   H�    � %               o%   o           o%   o           
"   
 d�           d    1�    d�    � %               o%   o           o%   o           
"   
 )�           �    1�    )� �   � %               o%   o           %               
"   
 )�           \    1� '   )� �   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 ]�           ,    1� 3   ]� �  	 � %               o%   o           �     ]
"   
 ��           �    1� @   �� �  	 � %               o%   o           �     ]
"   
 ]�               1� N   ]� �   � %               o%   o           %               
"   
 u�           �    1� \   u� �  	 � %               o%   o           �     ]
"   
 d�               1� k   d� �  	 � %               o%   o           �     u
"   
 *�           x    1� y   *� �   � %               o%   o           %               
"   
 :�           �    1� �   :� �  	 � %               o%   o           �     *
"   
 H�           h    1� �   H� �  	 � %               o%   o           �     :
"   
 ]�           �    1� �   ]� �  	 � %               o%   o           �     H
"   
 ]�           P     1� �   ]� �  	 � %               o%   o           o%   o           
"   
 ]�           �     1� �   ]� �  	 � %               o%   o           �     �
"   
 u�           @!    1� �   u� �  	 � %               o%   o           �     ]
"   
 d�           �!    1� �  	 d� �   � %               o%   o           %               
"   
 *�           0"    1� �   *� �   � %               o%   o           %               
"   
 *�           �"    1� �   *� �   � %               o%   o           o%   o           
"   
 :�           (#    1�    :� �   � %               o%   o           o%   o           
"   
 ]�           �#    1�    ]� �   � %               o%   o           %               
"   
 ��            $    1�     �� �   � %               o%   o           %               
"   
 ]�           �$    1� 1   ]� �   � %               o%   o           %               
"   
 u�           %    1� F   u� R   � %               o%   o           %       
       
"   
 u�           �%    1� Z   u� R   � %               o%   o           o%   o           
"   
 )�           &    1� f   )� R   � %               o%   o           %              
"   
 )�           �&    1� r   )� R   � %               o%   o           o%   o           
"   
 w�           '    1� ~   w� R   � %               o%   o           %              
"   
 w�           �'    1� �   w� R   � %               o%   o           o%   o           
"   
 ��            (    1� �   �� R   � %               o%   o           %              
"   
 ��           |(    1� �   �� R   � %               o%   o           o%   o           
"   
 u�           �(    1� �   u� �  	 � %               o%   o           �     HP �L 
�H T   %              �     }        �GG %              
"   
 d�           �)    1� �   d� �   � %               o%   o           %               
"   
 d�           <*    1� �   d� �   � %               o%   o           o%   o           
"   
 *�           �*    1� �   *�    � %               o%   o           �     )
"   
 w�           ,+    1� �   w�    � %               o%   o           � �  - *
"   
 ]�           �+    1� &   ]�    � %               o%   o           �     w
"   
 ��           ,    1� =   ��    � %               o%   o           � Z   ]
"   
 � �          �,    1� x   � � �     
"   
 H�           �,    1� �   H�    � %               o%   o           �     ]
"   
 � �          8-    1� �  
 � � �     
"   
 � �          t-    1� �   � � �     
"   
 *�           �-    1� �   *� �  	 � %               o%   o           �     )
"   
 w�           $.    1� �   w�    � %               o%   o           �     *
"   
 w�           �.    1� �   w� �   � %               o%   o           o%   o           
"   
 ��           /    1� �   ��    � %               o%   o           � �  ! :
"   
 H�           �/    1� 	   H�    � %               o%   o           �     �
"   
 u�           �/    1�    u�    � %               o%   o           � )   H
"   
 u�           p0    1� 8  	 u� �   � %               o%   o           o%   o           
"   
 )�           �0    1� B   )� �   � %               o%   o           %               
"   
 � �          h1    1� N   � � �     
"   
 w�           �1    1� \   w�    � %               o%   o           � p   ]
"   
 :�           2    1�    :� �  	 � %               o%   o           �     w
"   
 ��           �2    1� �   �� �  	 � %               o%   o           �     :
"   
 � �           3    1� �   � � �     
"   
 � �          <3    1� �   � � �  	   
"   
 u�           x3    1� �   u� �   � o%   o           o%   o           %               
"   
 � �          �3    1� �   � � �     
"   
 � �          04    1� �   � � �  	   
"   
 � �          l4    1� �   � � �  	   
"   
 � �          �4    1�    � � �  	   
"   
 � �          �4    1� !   � � �  	   
"   
 � �           5    1� 2   � � �  	   
"   
 � �          \5    1� C   � � �     
"   
 ��           �5    1� T   ��    � %               o%   o           � k  4 d
"   
 � �          6    1� �   � � �     
"   
 � �          H6    1� �   � � �     
"   
 � �          �6    1� �   � � �     
"   
 � �          �6    1� �   � � �  	   
"   
 � �          �6    1� �   � � �  	   
"   
 � �          87    1� �   � � �  	   
"   
 � �          t7    1�     � � �     
"   
 *�           �7    1�     *� �  	 � %               o%   o           �     w
"   
 H�           $8    1�     H� �  	 � %               o%   o           �     *
"   
 d�           �8    1� )    d� �  	 � %               o%   o           �     H
"   
 ��           9    1� >    �� �  	 � %               o%   o           �     d
"   
 ]�           �9    1� S    ]� �   � %               o%   o           %               
"   
 ]�           �9    1� a    ]� �   � %               o%   o           o%   o           
"   
 :�           x:    1� s    :� �   � %               o%   o           %               
"   
 w�           �:    1� �    w� �   � %               o%   o           %               
"   
 w�           p;    1� �    w� �   � %               o%   o           o%   o           
"   
 H�           �;    1� �    H� �   � %               o%   o           %               
"   
 � �          h<    1� �    � � �  	   
"   
 H�           �<    1� �    H� �   � %               o%   o           %              
"   
 � �           =    1� �    � � �  	   
"   
 � �          \=    1� �    � � �  	   
"   
 � �          �=    1� �   
 � � �  	   
"   
 w�           �=    1� �    w� �  	 � %               o%   o           � S    )
"   
 ]�           H>    1� !   ]� �  	 � %               o%   o           �     wP �L 
�H T   %              �     }        �GG %              
"   
 H�           ?    1�  !   H�    � %               o%   o           �     H
"   
 ��           �?    1� .!   �� �   � %               o%   o           %               
"   
 :�            @    1� ;!   :�    � %               o%   o           �     �
"   
 )�     ,      t@    1� K!   )�    � %               o%   o           �   � �     � [!   ��    	 )
"   
 :�           A    1� ]!   :� �   � %               o%   o           o%   o           
"   
 )�           �A    1� f!   )�    � %               o%   o           �     *
"   
 ]�           �A    1� t!   ]�    � %               o%   o           �     )
"   
 ]�           lB    1� �!   ]� �  	 � %               o%   o           o%   o           
"   
 ]�           �B    1� �!   ]�    � %               o%   o           o%   o           
"   
 u�           dC    1� �!   u�    � %               o%   o           �     H
"   
 d�           �C    1� �!   d� �   � %               o%   o           %               
"   
 � �          TD    1� �!   � � �     
"   
 *�           �D    1� �!   *�    � %               o%   o           � �!  ~ :
"   
 H�           E    1� n"   H�    � %               o%   o           �     *
"   
 w�           xE    1� �"   w�    � %               o%   o           � �"   H
"   
 ]�           �E    1� �"   ]� �  	 � %               o%   o           � �"   w
"   
 H�           `F    1� �"   H� �  	 � %               o%   o           � �"   ]
"   
 ��           �F    1� �"  	 ��    � %               o%   o           � �"   H
"   
 d�           HG    1� �"  
 d� �  	 � %               o%   o           � �"   �
"   
 d�           �G    1�  #   d� �   � %               o%   o           o%   o           
"   
 *�           8H    1� #   *�    � %               o%   o           � #   :
"   
 H�           �H    1� 1#   H�    � %               o%   o           �     *
"   
 H�            I    1� :#  
 H� �   � %               o%   o           o%   o           
"   
 � �          �I    1� E#   � � �     
"   
 H�           �I    1� S#   H�    � %               o%   o           � g#  ] ]
"   
 )�           LJ    1� �#   )�    � %               o%   o           �     H
"   
 d�           �J    1� �#   d�    � %               o%   o           � �#   )
"   
 :�           4K    1� �#   :� �   � %               o%   o           %               
"   
 ]�           �K    1� �   ]�    � %               o%   o           �     :
"   
 ]�           $L    1� �#   ]�    � %               o%   o           o%   o           
"   
 � �          �L    1� 	$   � � �  	   P �L 
�H T   %              �     }        �GG %              
"   
 ]�           0M    1� $   ]� �   � %               o%   o           %               
"   
 H�           �M    1� -$  	 H� �   � %               o%   o           %               
"   
 � �          (N    1� 7$   � �          
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              
�             �G "    � %     start-super-proc � %     adm2/smart.p ��P �L 
�H T   %              �     }        �GG %              
"   
   �       P    6� O     
"   
   
�        DP    8
"   
   �        dP    ��     }        �G 4              
"   
 ߱G %              G %              %� � �   EnabledObjFldsToDisable,ModifyFields,DataSourceNames,UpdateTargetNames,LogicalObjectName,LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        R    �� O   � P   �        R    �@    
� @  , 
�       R    �� X   �p�               �L
�    %              � 8      (R    � $         � _          
�    � y   �
"   
 �p� @  , 
�       8S    �� �   �p�               �L"    , �   � r$   :� t$   � �     }        �A      |    "      � r$   �%              (<   \ (    |    �     }        �A� v$   �A"  	  :    "    �"  	  :  < "    �"  	  :(    |    �     }        �A� v$   �A"  	  :
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        U    �� O   � P   �        U    �@    
� @  , 
�       $U    �� X   �p�               �L
�    %              � 8      0U    � $         � _          
�    � y   �
"   
 �p� @  , 
�       @V    ��   
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 w(�  L ( l       �        �V    �� O   � P   �        �V    �@    
� @  , 
�       �V    �� X   �p�               �L
�    %              � 8      W    � $         � _   �     
�    � y   � 
"   
 �p� @  , 
�       X    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        �X    �� O   � P   �        �X    �@    
� @  , 
�       �X    �� X     p�               �L
�    %              � 8      �X    � $         � _          
�    � y     
"   
 �p� @  , 
�       �Y    �� 3  
 �p�               �L%     SmartDataViewer 
"   
   p� @  , 
�       `Z    �� N     p�               �L%      FRAME   
"   
  p� @  , 
�       �Z    �� Q    p�               �L%               
"   
  p� @  , 
�        [    �� /    p�               �L(        �       �       �       �     }        �A
�H T   %              �     }        �GG %              
"   
 d (   � 
"   
 �    �         \    �� O   �
"   
   � 8      L\    � $         � _          
�    � y   �
"   
   �        �\    �
"   
   �       �\    /
"   
   
"   
   �       �\    6� O     
"   
   
�        ]    8
"   
   �        <]    �
"   
   �       \]    �
"   
   p�    � �$   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �         ^    �A"    �A
"   
   
�        l^    �@ � 
"   
 d"      �       }        �
"   
 � %              %                "    � %     start-super-proc � %     adm2/appserver.p A��    �  %     
�    �     }        �%               %      Server  - �     }        �    "  
  ]�     � %                   "    ]�     � %      NONE    p�,  8         $     "    w        � :%   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �`    �� O   � P   �        �`    �@    
� @  , 
�       �`    �� X   �p�               �L
�    %              � 8      �`    � $         � _          
�    � y   �
"   
 �p� @  , 
�       �a    �� �   �p�               �L"    , p�,  8         $     "  
  w        � H%   �
�     "    � %     start-super-proc � %     adm2/visual.p �� 
"    
 � %     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP ��%     processAction   
�    %     CTRL-PAGE-DOWN  "    � %     start-super-proc � %     adm2/containr.p %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents :%      initializeDataObjects :0 0   A    �    � �%   :
�    � �%   � A    �    � �%     
�    � �%   � %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents }%     buildDataRequest ent0 A    �    � �%   � 
�    � �%   ]%     modifyListProperty  
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 }(�  L ( l       �        hf    �� O   � P   �        tf    �@    
� @  , 
�       �f    �� X   �p�               �L
�    %              � 8      �f    � $         � _   �     
�    � y   � 
"   
 �p� @  , 
�       �g    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        Hh    �� O   � P   �        Th    �@    
� @  , 
�       `h    �� X   �p�               �L
�    %              � 8      lh    � $         � _   �     
�    � y   �
"   
 �p� @  , 
�       |i    �� S    �p�               �L%               "    � %     start-super-proc � %     adm2/datavis.p %     modifyListProperty  
�    %      ADD     %     SupportedLinks %     Toolbar-Target %     valueChanged    
�    %     valueChanged    
�     "    � %     start-super-proc � %     adm2/viewer.p �%     modifyListProperty  
�    
�    %      Add     %     DataSourceEvents i]%     buildDataRequest i]�   � �   H� [!     � �&  � ��   � �     � [!   �� �&  � H�@    �    � �   �� '   d     � �   �"    d� �   � �@    �    � �     � '         � �   d"    � � �     
�H T   %              �     }        �GG %              
"   
 � 
"   
 �
"   
 � 
"   
 � (�  L ( l       �         m    �� O   � P   �        m    �@    
� @  , 
�       m    �� X   � p�               �L
�    %              � 8      $m    � $         � _   �      
�    � y     
"   
 �p� @  , 
�       4n    �� ;!   �p�               �L"    , 
"   
   p� @  , 
�       �n    �� f!     p�               �L"    , 
"   
  p� @  , 
�       �n    �� :#  
  p�               �L%               �             I%               �             �%              �     }        �
�                    �           �   l       ��                 &  J  �               ��6                    O   ����    e�          O   ����    R�          O   ����    ��        $  5  �   ���                       �[     
                    � ߱              6  (  �      \      4   ����\                �                      ��                  7  I                  M                       7  8  �  �  8  X\            :  �  `      �\      4   �����\                p                      ��                  ;  H                  �p�                       ;  �  �  o   <      ,                                 �  �   =  �\      �  �   >  �\      $  $  ?  �  ���                       (]     
                    � ߱        8  �   @  H]      L  �   A  h]      `  �   D  �]          $   G  �  ���                       �]  @         �]              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 n  �  �               �q�                    O   ����    e�          O   ����    R�          O   ����    ��      �$                      �          �  $  �    ���                       ^     
                    � ߱                  �  �                      ��                   �  �                  �t�                     �  4      4   ����,^      $  �  �  ���                       x^     
                    � ߱        �    �  4  D      �^      4   �����^      /  �  p                               3   �����^  �  �   �  �^          O   �  ��  ��  �^                               , �                          
                               �      ��                            ����                                                        �   l       ��                  �  �  �               y                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  W  c  �               ��<                    O   ����    e�          O   ����    R�          O   ����    ��      �      a  �� �                       b  �         �o      4   �����o      �   b  �o    ��                              ��        )                  ����                               !   d d     �   ��,  ,  � �                                               )      �                                                                  d     D                                                                 P   xd �d                                                           8'  G   
 X xd �d                                                        ]      0     g L   `  @       #      Cust Num %               � 6  )    P   x� fd                                                           A'  G   
 X x� Gd                                             
                �     g     �       P   x,.d                                                           F'  G   
 X x,;d                                                             �  #   g     �       P   x�<d                                                           N'  G   
 X x�d                                                       n      �  2   g     �       P   x��d                                                           T'  G   
 X x�d                                                       Q      �  
   g �   #  �   4 4       #     Credit Limit %                   #     Credit Limit %       ��     � �  *    P   xX�d                                                           a'  G   
 X xX_d                                                       �      @     g     F       P   x��d                                                           g'  G   
 X x�Kd                                                       �      b     g $   �     V p
  � p  E    P   x  d                                                           q'  G   
 X x ]d                                                       +      �     g     �        D                                                                    TXS appSrvUtils RowObject Address Address2 Balance City Comments Contact Country CreditLimit CustNum Discount EmailAddress Fax Name Phone PostalCode SalesRep State Terms ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST F-Main >>>>9 Customer number must be greater than zero Please enter a customer number. x(30) Please enter a name. x(35) Please enter an address. x(50) Please enter an full Internet Email Address. ->,>>>,>>9 Credit Limit must be >= 0 and <= 9,999,999 Please enter a Credit Limit. x(20) Please enter a phone number x(4) Salesrep The Sales Rep's name you've entered must exist in the Salesrep table. Please Enter a Sales Rep. ->,>>>,>>9.99 Please enter a balance. C:\newsie\on_in_co\aplic\vcustomer.w should only be RUN PERSISTENT. GETTARGETPROCEDURE GETOBJECTTYPE GETSHOWPOPUP SETSHOWPOPUP GETCREATEHANDLES GETDATAMODIFIED GETDISPLAYEDFIELDS GETDISPLAYEDTABLES GETENABLEDFIELDS GETENABLEDHANDLES GETFIELDHANDLES GETFIELDSENABLED GETGROUPASSIGNSOURCE GETGROUPASSIGNSOURCEEVENTS GETGROUPASSIGNTARGET GETGROUPASSIGNTARGETEVENTS GETNEWRECORD GETOBJECTPARENT GETRECORDSTATE GETROWIDENT GETTABLEIOSOURCE GETTABLEIOSOURCEEVENTS GETUPDATETARGET GETUPDATETARGETNAMES GETWINDOWTITLEFIELD OKTOCONTINUE SETCONTAINERMODE SETDATAMODIFIED SETDISPLAYEDFIELDS SETENABLEDFIELDS SETGROUPASSIGNSOURCE SETGROUPASSIGNSOURCEEVENTS SETGROUPASSIGNTARGET SETGROUPASSIGNTARGETEVENTS SETLOGICALOBJECTNAME SETOBJECTPARENT SETSAVESOURCE SETTABLEIOSOURCE SETTABLEIOSOURCEEVENTS SETUPDATETARGET SETUPDATETARGETNAMES SETWINDOWTITLEFIELD SHOWDATAMESSAGES DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETWAITFOROBJECT SETWINDOWTITLEVIEWER SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALVERSION SETOBJECTNAME SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataViewer ContainerType FRAME PropertyDialog adm2/support/viewerd.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties EnabledObjFldsToDisable,ModifyFields,DataSourceNames,UpdateTargetNames,LogicalObjectName,LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName CreateHandles DataModified DisplayedFields DisplayedTables   Editable EnabledFields EnabledHandles EnabledObjFldsToDisable EnabledWhenNew FieldHandles FieldsEnabled GroupAssignSource GroupAssignSourceEvents addRecord,copyRecord,updateRecord,resetRecord,undoRecord,cancelRecord,enableFields,disableFields,collectChanges,validateFields GroupAssignTarget GroupAssignTargetEvents updateState,LinkState InternalDisplayFromSource (Large) ModifyFields (All) NewRecord No ObjectMode View PrintPreviewActive RecordState NoRecordAvailable RowIdent SaveSource TableIOSource TableIOSourceEvents addRecord,updateRecord,copyRecord,deleteRecord,resetRecord,undoChange,cancelRecord,updateMode ToolbarSource ToolbarSourceEvents toolbar UndoNew UpdateTargetNames WindowTitleField KeepChildPositions ShowPopup FieldWidgetIDs ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target adm2/datavis.p ADD Toolbar-Target DISPLAYOBJECTS cViewCols cEnabled iCol iEntries cEntry adm2/viewer.p RowObject.CustNum RowObject.Name RowObject.Address RowObject.EmailAddress RowObject.CreditLimit RowObject.Phone RowObject.SalesRep RowObject.Balance ,RowObject. DISABLE_UI default Cust Num Name Address Email Credit Limit Phone Sales Rep Balance �  `#  �   ,      3 �    ��      0         pcFieldType     ��      T         pcColValues     ��      x         pcValue     ��      �         pcState �   ��      �         pcChanges       ��      �         pcChanges       ��               plCancel        ��      $        plAnswer        ��      H        plCancel        ��      l        pcRelative  �  ��      �        pcAction        ��      �        pcAction        ��      �        pcState     ��      �        pcReturn        ��              pcMode      ��      <        pcState     ��      \        pcNotValidFields    �  ��      �        pcProp      ��      �        pcProp      ��      �        plCancel    �  ��      �        pcProcName    ��             
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             	     cType       T	     T   �          D	                  getObjectType   6  N  P  �	        t	  
   hReposBuffer    �	        �	  
   hPropTable  �	        �	  
   hBuffer           �	  
   hTable  	  
     U   `	          
                  adm-clone-props 5  6  7  8  :  ;  <  =  >  ?  @  A  D  G  H  I  J            t
  
   hProc             �
        pcProcName  �	  �
  	   V   `
  |
      �
                  start-super-proc    �  �  �  �  �  �  �  �  �  �
  8     W                                   Y    l     X                                   ]  ^  <  �     Y                                   a  b  t  �     Z               �                  displayObjects  �  �        [                                     �  T     \                                     $  �     ]               �                  disable_UI  a  b  c  X  T  %    
 p      8                          �  �     RowObject   �         �         �         �         �         �         �                                    $         4         8         @         H         T         `         h         Address Address2    Balance City    Comments    Contact Country CreditLimit CustNum Discount    EmailAddress    Fax Name    Phone   PostalCode  SalesRep    State   Terms   �          �  
   appSrvUtils �        �  
   gshAstraAppserver   �        �  
   gshSessionManager           �  
   gshRIManager    ,          
   gshSecurityManager  T        @  
   gshProfileManager   �  	 	     h  
   gshRepositoryManager    �  
 
     �  
   gshTranslationManager   �        �  
   gshWebManager   �        �     gscSessionId                 gsdSessionObj   <        ,  
   gshFinManager   `        P  
   gshGenManager   �        t  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj  �        �     gsdRenderTypeObj                 gsdSessionScopeObj  4       ,  
   ghProp  T       H  
   ghADMProps  x       h  
   ghADMPropsBuf   �       �     glADMLoadFromRepos  �       �     glADMOk �       �  
   ghContainer �       �     cObjectName     	        iStart  8    
   ,     cAppService X       L     cASDivision �       l     cServerOperatingMode    �       �     cFields �       �     cViewCols   �       �     cEnabled    �       �     iCol                iEntries             0     cEntry        X  H  RowObject            S   -  `  a  c  d  
  
  
  
  
  *
  +
  ,
  .
  0
  1
  2
  6
  7
  :
  ;
  <
  =
  ?
  A
  C
  E
  F
  G
  J
  L
  M
  O
  P
  Q
  R
  S
  Y
  [
  a
  c
  e
  f
  l
  m
  n
  o
  r
  s
  u
  v
  x
  y
  z
  {
  |
  }
  ~
  �
  �
  �
  �
  �
  �
  �
  �
  �
  p  q  t  u  v  w  x  y  z  {  |  }  ~    �  �  �              	  
                                               �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  O  ]  ^  `  a  b  c  i  j  k  l  m  n  o  p  q  s  t  u  v  w  x  y  z  {  |  }  ~  �  �  �  �  �  �  �            {  =  >  @  A  D  E  G  O  �  �  �  �  �  �  �  �  �  �  �  �  �      .  �  �  �  �  �  �  �  �  �         %  9  l  �  �  V  W  X  Z  \  `  y  z  {  }  �  �  �  �  �  �    :  k  l  m  o  q         !  "  #  (  -  =  1      :%  C:\Progress\OpenEdge\src\adm2\viewer.i   �  �Q 2 %C:\Progress\OpenEdge\src\adm2\custom\viewercustom.i    } & C:\Progress\OpenEdge\src\adm2\datavis.i  \  � 1 %C:\Progress\OpenEdge\src\adm2\custom\dataviscustom.i �  f! ' C:\Progress\OpenEdge\src\adm2\containr.i �  � 0 %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i      �� ( C:\Progress\OpenEdge\src\adm2\visual.i   H  # / %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  |  �< ) C:\Progress\OpenEdge\src\adm2\appserver.i    �  �� . %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �  I� * C:\Progress\OpenEdge\src\adm2\smart.i    8  Ds - C:\Progress\OpenEdge\gui\fn  l  tw , %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �  Q. + C:\Progress\OpenEdge\gui\set �  �/  C:\Progress\OpenEdge\src\adm2\viewprop.i �  �� $ %C:\Progress\OpenEdge\src\adm2\custom\viewpropcustom.i    0  ۃ % %C:\Progress\OpenEdge\src\adm2\custom\viewprtocustom.i    t  ��  C:\Progress\OpenEdge\src\adm2\dvisprop.i �  B� " %C:\Progress\OpenEdge\src\adm2\custom\dvispropcustom.i    �  �� # %C:\Progress\OpenEdge\src\adm2\custom\dvisprtocustom.i    0  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i t  ��   %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �  P ! %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  0  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i d  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i      V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    \  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �  �j  C:\Progress\OpenEdge\gui\get �  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    @  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   ,   �  C:\Progress\OpenEdge\src\adm2\appsprto.i p   ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �   �X  C:\Progress\OpenEdge\src\adm2\visprto.i  �   !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i   !  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i d!  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �!  �7 
 C:\Progress\OpenEdge\src\adm2\dvisprto.i �!  0 	 %C:\Progress\OpenEdge\src\adm2\custom\datavisdefscustom.i "  ��  C:\Progress\OpenEdge\src\adm2\viewprto.i X"  gf  %C:\Progress\OpenEdge\src\adm2\custom\viewerdefscustom.i  �"  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �"  �D  .\aplic\dcustomer.i  #  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   (#  
�    C:\newsie\on_in_co\aplic\vcustomer.w     �   #      �#  �   �     �#     �  2   �#  �   �     �#     }  +   �#  �   z     �#     X  +   �#  �   W      $     5  +   $  \         $  o   �  &   0$     u  1   @$  U   [  &   P$  �   T  '   `$     2  +   p$  �   -  '   �$       +   �$  �     '   �$     �  0   �$  �   �  '   �$     �  -   �$  �   �  '   �$     �  -   �$  �   �  '    %     �  -   %  r   j  '    %  n   R  (   0%     �  /   @%  P   �  (   P%  �   �  )   `%     {  .   p%  �   v  )   �%     T  +   �%  �   S  )   �%     1  +   �%  �   /  )   �%       +   �%  g   �  )   �%     �     �%  O   �  )    &  �   F  *   &     D  -    &  �     *   0&     �  ,   @&  �   �  *   P&     �  +   `&  �   �  *   p&     l  +   �&  �   k  *   �&     I  +   �&  �   H  *   �&     &  +   �&  �     *   �&     �  +   �&  �   �  *   �&     �  +    '  }   �  *   '     �  +    '     $  *   0'     �  )   @'     �  (   P'       '   `'     �  &   p'     �     �'  u   {     �'  O   m  $   �'     \  %   �'       $   �'  h        �'  �   �     �'  O   �  "   �'     �  #    (     �  "   (  {   X      (  �   O     0(  O   A      @(     0  !   P(     �      `(  �   �     p(  �   �     �(  O   �     �(     r     �(     $     �(  �   �     �(  x   �     �(  M   �     �(     �     �(     �      )  a   n     )  �  M      )     .     0)  �  �
     @)  O   �
     P)     �
     `)     �
     p)  �   �	     �)     �     �)     �     �)  x   �     �)     �     �)     I     �)     E     �)     1     �)           *  Q        *     �      *     v     0*     b     @*     H     P*  f        `*     �     p*  "   x     �*     d     �*     C     �*  Z   �     �*     �     �*     �     �*     �     �*     �     �*  X   j      +     �  
   +      |      +     h  	   0+     I     @+  ]   >     P+          `+     �     p+     �     �+     �     �+     w     �+  0   �       �+     \      �+     9       �+     &      �+     !       �+           