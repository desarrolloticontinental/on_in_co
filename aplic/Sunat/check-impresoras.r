	��V�8�a5  � �              /                                �� 3510010Dutf-8 MAIN d:\newsie\on_in_co\APLIC\sunat\check-impresoras.w,, PROCEDURE show-attributos,, PROCEDURE exitObject,, PROCEDURE enumerateprinters,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE carga-impresoras,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE initializeObject,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER       �              4�              �� �  @�              8l              `+  	  +   lk �  7   p `  8   ls �   E   `t  	  F   �} |  G   �~ �  H   ��   I   ̅ $  J   �� �  K           �� �  ? |� !  iSO8859-1                                                                           P    �                                       �                  ��   	 �              �     �   �    ��  �         ��  �   �      �                                                        PROGRESS                         �           
    
                    �              �                                                                                                     
  h  �                 `  �
      �  
    
                  �  �             L                                                                                          �
          
    �
      �  
    
                  t  <             �                                                                                          �
          
  �  �
      4  
    
                     �             �                                                                                          �
          
  d  �
      �  
    
                  �  �             P                                                                                          �
          
    �
      �  
    
                  x  @             �                                                                                          �
          
  �  �
      8  
    
                  $  �             �                                                                                          �
          
  h        �  
    
                  �  �  	           T                                                                                                    
    !      �  
    
                  |  D  
                                                                                                      !          
  �  /      <                         (  �             �                                                                                          /            l  <      �                        �  �             X                                                                                          <            	  J      �  
    
                  �  H	             	                                                                                          J          
  �	  X      @	  
    
                  ,	  �	             �	                                                                                          X          
  p
  f      �	  
    
                  �	  �
             \
                                                                                          f          
    t      �
                        �
  L                                                                                                       t            �  �      D                        0  �             �                                                                                          �            t  �      �                        �  �             `                                                                                          �                �      �                        �  p                                                                                                       �                         INTEGRAL                         PROGRESS                                L         L                         �a�a            L  �                              �  �                      �     ~~     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2CODCAJADCTO_OTROS_MOTDCTO_OTROS_FACTORDCTO_OTROS_VVDCTO_OTROS_PVLISTA_DE_PRECIOSTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �  �     F           �  �     F               U      I                        �                                              . �          8  �  � �                                                                                                                                                       
                BOL                                                                                                                                   
             
             
                                         
                                                                                                               $ �   �   �   �   �   �   �   �       ,  <  L  \  l  |  �  �  �  �  �  �  �  �      ,  <  L  \  l  |  �  �     $ �   �   �   �   �   �   �   �      ,  <  L  \  l  |  �  �  �  �  �  �  �  �      ,  <  L  \  l  |  �  �    ��                                               �          ����                            !   �"    undefined                                                               �       <�  �   l   L�    \�                  �����               l>\                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     7          assignFocusedWidget         �      �            LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    $       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    6       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          L       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    X       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    d       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    w       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    #      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    7      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    E      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    U      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    f      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    s      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    �    �  �
  �
  P  �       4   �����       o   �       �
                              �  �   NA  �   �  �   �  �      �          $    8    L    `    t  `  �  
`  �  $  �    �     �      $  �  |  ���                       �     
                    � ߱        ؁    �  �  @      �      4   �����                P                      ��                  �  �                  ,�^                       �  �  �    �  l  |      (      4   ����(      $  �  �  ���                       x  @         d              � ߱              �  �         �      4   �����      $  �  ,  ���                         @         �              � ߱        assignPageProperty                              �  �      ��                  }  �                ��_                    O   ����    e�          O   ����    R�          O   ����    ��            ��   T                             ��                  H           ��                            ����                            changePage                              @  (      ��                  �  �  X              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             @  (      ��                  �  �  X              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  p           ��                            ����                            constructObject                             l  T      ��                  �  �  �               �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
  �             �  
             ��                 �               �� 
                   
         ��                            ����                            createObjects                                 �      ��                  �  �  (              $                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                                �      ��                  �  �  (              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            destroyObject                               <  $      ��                  �  �  T              $�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                <  $      ��                  �  �  T              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            initializeObject                                l  T      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               |  d      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               |  d      ��                  �  �  �              D1                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            notifyPage                              �  �      ��                  �  �  �              ,x                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            passThrough                             �  �      ��                  �  �  �              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   0             �               ��                  $           ��                            ����                            removePageNTarget                               $        ��                  �  �  <              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             T  
             ��                  |           ��                            ����                            selectPage                              t  \      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            toolbar                             �  �      ��                  �  �  �              T�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            viewObject                              �   �       ��                  �  �  �               �$                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                �!  �!      ��                  �  �  �!              L%                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �!           ��                            ����                            disablePagesInFolder    
      X"      �"    �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder p"      �"      �"          LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  �"      #      P#    #      HANDLE, getCallerWindow 0#      X#      �#    6      HANDLE, getContainerMode    h#      �#      �#    F      CHARACTER,  getContainerTarget  �#      �#      $    W      CHARACTER,  getContainerTargetEvents    �#      $      L$    j      CHARACTER,  getCurrentPage  ,$      X$      �$    �      INTEGER,    getDisabledAddModeTabs  h$      �$      �$     �      CHARACTER,  getDynamicSDOProcedure  �$      �$      %  !  �      CHARACTER,  getFilterSource �$      %      L%  "  �      HANDLE, getMultiInstanceActivated   ,%      T%      �%  #  �      LOGICAL,    getMultiInstanceSupported   p%      �%      �%  $  �      LOGICAL,    getNavigationSource �%      �%      &  %        CHARACTER,  getNavigationSourceEvents   �%      $&      `&  &        CHARACTER,  getNavigationTarget @&      l&      �&  '  2      HANDLE, getOutMessageTarget �&      �&      �&  (  F      HANDLE, getPageNTarget  �&      �&      '  )  Z      CHARACTER,  getPageSource   �&       '      P'  *  i      HANDLE, getPrimarySdoTarget 0'      X'      �'  +  w      HANDLE, getReEnableDataLinks    l'      �'      �'  ,  �      CHARACTER,  getRunDOOptions �'      �'      (  -  �      CHARACTER,  getRunMultiple  �'      (      D(  .  �      LOGICAL,    getSavedContainerMode   $(      P(      �(  /  �      CHARACTER,  getSdoForeignFields h(      �(      �(  0  �      CHARACTER,  getTopOnly  �(      �(       )  1 
 �      LOGICAL,    getUpdateSource �(      )      <)  2  �      CHARACTER,  getUpdateTarget )      H)      x)  3        CHARACTER,  getWaitForObject    X)      �)      �)  4        HANDLE, getWindowTitleViewer    �)      �)      �)  5  %      HANDLE, getStatusArea   �)       *      0*  6  :      LOGICAL,    pageNTargets    *      <*      l*  7  H      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject L*      �*      �*  8  U      LOGICAL,INPUT h HANDLE  setCallerProcedure  �*      �*       +  9  e      LOGICAL,INPUT h HANDLE  setCallerWindow  +      8+      h+  :  x      LOGICAL,INPUT h HANDLE  setContainerMode    H+      �+      �+  ;  �      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  �+      �+      ,  <  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �+      4,      d,  =  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  D,      �,      �,  >  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �,      �,       -  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource  -      @-      p-  @  �      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  P-      �-      �-  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �-      �-       .  B        LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported    .      P.      �.  C  &      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource l.      �.      �.  D  @      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �.      /      P/  E  T      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget 0/      t/      �/  F  n      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �/      �/      �/  G  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �/      0      L0  H  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   ,0      p0      �0  I  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �0      �0      �0  J  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �0      1      T1  K  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget 41      �1      �1  L  �      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �1      �1       2  M  �      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �1      $2      T2  N  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   42      x2      �2  O        LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �2      �2      3  P  !      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �2      <3      h3  Q 
 5      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource H3      �3      �3  R  @      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget �3      �3      4  S  P      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    �3      04      d4  T  `      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    D4      �4      �4  U  q      LOGICAL,INPUT phViewer HANDLE   getObjectType   �4      �4      5  V  �      CHARACTER,  setStatusArea   �4      5      H5  W  �      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �5  �5      ��                  ?  @  6              (<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                                7  �6      ��                  B  C  7              @�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                8  �7      ��                  E  F  8              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                9  �8      ��                  H  I  $9              x�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               :  �9      ��                  K  M  (:              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @:           ��                            ����                            getAllFieldHandles  (5      �:      �:  X  �      CHARACTER,  getAllFieldNames    �:      �:      ;  Y  �      CHARACTER,  getCol  �:      (;      P;  Z  �      DECIMAL,    getDefaultLayout    0;      \;      �;  [  �      CHARACTER,  getDisableOnInit    p;      �;      �;  \  �      LOGICAL,    getEnabledObjFlds   �;      �;      <  ]  �      CHARACTER,  getEnabledObjHdls   �;      <      P<  ^        CHARACTER,  getHeight   0<      \<      �<  _ 	       DECIMAL,    getHideOnInit   h<      �<      �<  `        LOGICAL,    getLayoutOptions    �<      �<      =  a  +      CHARACTER,  getLayoutVariable   �<      =      D=  b  <      CHARACTER,  getObjectEnabled    $=      P=      �=  c  N      LOGICAL,    getObjectLayout d=      �=      �=  d  _      CHARACTER,  getRow  �=      �=      �=  e  o      DECIMAL,    getWidth    �=       >      ,>  f  v      DECIMAL,    getResizeHorizontal >      8>      l>  g        LOGICAL,    getResizeVertical   L>      x>      �>  h  �      LOGICAL,    setAllFieldHandles  �>      �>      �>  i  �      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �>      ?      @?  j  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout     ?      `?      �?  k  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    t?      �?      �?  l  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �?      @      <@  m  �      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    @      \@      �@  n  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout p@      �@      �@  o  
	      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �@      A      <A  p  	      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   A      hA      �A  q  .	      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated |A      �A      �A  r  @	      LOGICAL,    getObjectSecured    �A      B      8B  s  T	      LOGICAL,    createUiEvents  B      DB      tB  t  e	      LOGICAL,    bindServer                              C  �B      ��                  /  0  (C              |�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               D  �C      ��                  2  3  ,D              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             E  E      ��                  5  6  4E              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                $F  F      ��                  8  9  <F               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              0G  G      ��                  ;  <  HG              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             8H   H      ��                  >  ?  PH              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             <I  $I      ��                  A  C  TI              lI                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 lI  
         ��                            ����                            startServerObject                               lJ  TJ      ��                  E  F  �J              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                pK  XK      ��                  H  J  �K              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �K           ��                            ����                            getAppService   TB      L      8L  u  t	      CHARACTER,  getASBound  L      DL      pL  v 
 �	      LOGICAL,    getAsDivision   PL      |L      �L  w  �	      CHARACTER,  getASHandle �L      �L      �L  x  �	      HANDLE, getASHasStarted �L      �L      M  y  �	      LOGICAL,    getASInfo   �L      (M      TM  z 	 �	      CHARACTER,  getASInitializeOnRun    4M      `M      �M  {  �	      LOGICAL,    getASUsePrompt  xM      �M      �M  |  �	      LOGICAL,    getServerFileName   �M      �M      N  }  �	      CHARACTER,  getServerOperatingMode  �M       N      XN  ~  �	      CHARACTER,  runServerProcedure  8N      dN      �N    
      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   xN      �N      O  �  !
      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �N      4O      dO  �  /
      LOGICAL,INPUT pcDivision CHARACTER  setASHandle DO      �O      �O  �  =
      LOGICAL,INPUT phASHandle HANDLE setASInfo   �O      �O       P  � 	 I
      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �O       P      XP  �  S
      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  8P      |P      �P  �  h
      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �P      �P       Q  �  w
      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �P      $Q      \Q  �  �
      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             R   R      ��                      0R              �i                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  |R             HR  
             ��   �R             pR               �� 
                 �R  
         ��                            ����                            addMessage                              �S  xS      ��                      �S              �a                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �S             �S               ��   T             �S               ��                  T           ��                            ����                            adjustTabOrder                              U  �T      ��                      $U              -                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  pU             <U  
             �� 
  �U             dU  
             ��                  �U           ��                            ����                            applyEntry                              �V  lV      ��                    !  �V              4                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �V           ��                            ����                            changeCursor                                �W  �W      ��                  #  %  �W              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �W           ��                            ����                            createControls                              �X  �X      ��                  '  (  �X              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �Y  �Y      ��                  *  +  �Y               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �Z  �Z      ��                  -  .  �Z              `_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �[  �[      ��                  0  1  \              �_                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �\  �\      ��                  3  4  ]              �`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �]  �]      ��                  6  7  ^              )                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �^  �^      ��                  9  :  _              �)                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                               `  �_      ��                  <  A  `              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  d`             0`  
             ��   �`             X`               ��   �`             �`               ��                  �`           ��                            ����                            modifyUserLinks                             �a  �a      ��                  C  G  �a              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   b             �a               ��   0b             �a               �� 
                 $b  
         ��                            ����                            removeAllLinks                               c  c      ��                  I  J  8c              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                               d  d      ��                  L  P  8d              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �d             Pd  
             ��   �d             xd               �� 
                 �d  
         ��                            ����                            repositionObject                                �e  �e      ��                  R  U  �e              ,�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   f             �e               ��                  �e           ��                            ����                            returnFocus                             �f  �f      ��                  W  Y  g              pJ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                  g  
         ��                            ����                            showMessageProcedure                                $h  h      ��                  [  ^  <h              �H                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �h             Th               ��                  |h           ��                            ����                            toggleData                              ti  \i      ��                  `  b  �i              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �i           ��                            ����                            viewObject                              �j  �j      ��                  d  e  �j              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  <Q      k      8k  � 
 �      LOGICAL,    assignLinkProperty  k      Dk      xk  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   Xk      �k       l  �        CHARACTER,  getChildDataKey �k      l      <l  �        CHARACTER,  getContainerHandle  l      Hl      |l  �  *      HANDLE, getContainerHidden  \l      �l      �l  �  =      LOGICAL,    getContainerSource  �l      �l      �l  �  P      HANDLE, getContainerSourceEvents    �l       m      <m  �  c      CHARACTER,  getContainerType    m      Hm      |m  �  |      CHARACTER,  getDataLinksEnabled \m      �m      �m  �  �      LOGICAL,    getDataSource   �m      �m      �m  �  �      HANDLE, getDataSourceEvents �m       n      4n  �  �      CHARACTER,  getDataSourceNames  n      @n      tn  �  �      CHARACTER,  getDataTarget   Tn      �n      �n  �  �      CHARACTER,  getDataTargetEvents �n      �n      �n  �  �      CHARACTER,  getDBAware  �n      �n      (o  � 
 �      LOGICAL,    getDesignDataObject o      4o      ho  �        CHARACTER,  getDynamicObject    Ho      to      �o  �        LOGICAL,    getInstanceProperties   �o      �o      �o  �  (      CHARACTER,  getLogicalObjectName    �o      �o      0p  �  >      CHARACTER,  getLogicalVersion   p      <p      pp  �  S      CHARACTER,  getObjectHidden Pp      |p      �p  �  e      LOGICAL,    getObjectInitialized    �p      �p      �p  �  u      LOGICAL,    getObjectName   �p      �p      ,q  �  �      CHARACTER,  getObjectPage   q      8q      hq  �  �      INTEGER,    getObjectParent Hq      tq      �q  �  �      HANDLE, getObjectVersion    �q      �q      �q  �  �      CHARACTER,  getObjectVersionNumber  �q      �q      $r  �  �      CHARACTER,  getParentDataKey    r      0r      dr  �  �      CHARACTER,  getPassThroughLinks Dr      pr      �r  �  �      CHARACTER,  getPhysicalObjectName   �r      �r      �r  �        CHARACTER,  getPhysicalVersion  �r      �r      (s  �        CHARACTER,  getPropertyDialog   s      4s      hs  �  ,      CHARACTER,  getQueryObject  Hs      ts      �s  �  >      LOGICAL,    getRunAttribute �s      �s      �s  �  M      CHARACTER,  getSupportedLinks   �s      �s       t  �  ]      CHARACTER,  getTranslatableProperties    t      ,t      ht  �  o      CHARACTER,  getUIBMode  Ht      tt      �t  � 
 �      CHARACTER,  getUserProperty �t      �t      �t  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �t      u      <u  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles u      du      �u  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    pu      �u      �u  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �u       v      Lv  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   ,v      �v      �v  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    �v      w      <w  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  w      dw      �w  �  �      CHARACTER,  setChildDataKey tw      �w      �w  �        LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �w      �w      ,x  �        LOGICAL,INPUT plHidden LOGICAL  setContainerSource  x      Lx      �x  �  +      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    `x      �x      �x  �  >      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �x       y      4y  �  W      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   y      \y      �y  �  k      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ly      �y      �y  �  y      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �y      z      <z  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   z      dz      �z  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents tz      �z      �z  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �z      {      <{  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject {      \{      �{  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    p{      �{      �{  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �{      |      @|  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName     |      d|      �|  �        LOGICAL,INPUT c CHARACTER   setLogicalVersion   ||      �|      �|  �        LOGICAL,INPUT cVersion CHARACTER    setObjectName   �|      }      @}  �  /      LOGICAL,INPUT pcName CHARACTER  setObjectParent  }      `}      �}  �  =      LOGICAL,INPUT phParent HANDLE   setObjectVersion    p}      �}      �}  �  M      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �}      ~      @~  �  ^      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks  ~      h~      �~  �  o      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   |~      �~      �~  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �~            H  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute (      l      �  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   |      �      �  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   �      �      X�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  8�      |�      ��  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      Ȁ      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ؀      8�      d�  �        LOGICAL,INPUT pcMessage CHARACTER   Signature   D�      ��      ��  � 	       CHARACTER,INPUT pcName CHARACTER    ��    {  �  p�      @      4   ����@                ��                      ��                  |  �                  D=                       |  �        }  ��  �      P      4   ����P                (�                      ��                  ~  �                  �=                       ~  ��  (�    �  D�  ��      d      4   ����d                Ѓ                      ��                  �  �                  lc                       �  T�         �                                        
                    � ߱        T�  $  �  ��  ���                           $  �  ��  ���                       L                         � ߱        ��    �  Ȅ  D�      \      4   ����\                T�                      ��                  �  r	                  d                       �  ؄  ��  o   �      ,                                 ��  $   �  ��  ���                       �  @         �              � ߱        �  �   �  �      �  �   �  d      �  �   �  �      0�  �   �  L      D�  �   �  �      X�  �   �  4      l�  �   �  �      ��  �   �  �      ��  �   �  `      ��  �   �  �      ��  �   �  P	      І  �   �  �	      �  �   �  H
      ��  �   �  �
      �  �   �          �  �   �  t      4�  �   �  �      H�  �   �  $      \�  �   �  `      p�  �   �  �      ��  �   �  H      ��  �   �  �      ��  �   �  @      ��  �   �  �      ԇ  �   �  0      �  �   �  �      ��  �   �        �  �   �  T      $�  �   �  �      8�  �   �        L�  �   �  x      `�  �   �  �      t�  �   �  �      ��  �   �  ,      ��  �   �  h      ��  �   �  �      Ĉ  �   �         ؈  �   �  \      �  �   �  �       �  �   �  �      �  �   �        (�  �   �  L      <�  �   �  �      P�  �   �  �          �   �                         |�          �  Љ      ��                  �	  �	   �              �f                    O   ����    e�          O   ����    R�          O   ����    ��      p     
                �                     �                         � ߱        ��  $ �	  �  ���                           O   �	  ��  ��  <               �          �  �    �                                             ��                            ����                                �4      d�      ��     6     �                      V �  �                     x�    �	  ԋ  P�      H      4   ����H                `�                      ��                  �	  n
                  ��                       �	  �  t�  �   �	  �      ��  �   �	        ��  �   �	  �      ��  �   �	        Č  �   �	  �      ،  �   �	        �  �   �	  �       �  �   �	  �      �  �   �	  x      (�  �   �	  �      <�  �   �	  h      P�  �   �	  �      d�  �   �	  `          �   �	  �      P�    y
  ��  �      L      4   ����L                 �                      ��                  z
                    |�                       z
  ��  4�  �   |
  �      H�  �   }
         \�  �   ~
  �      p�  �   
         ��  �   �
  �       ��  �   �
  �       ��  �   �
  t!      ��  �   �
  �!      Ԏ  �   �
  \"      �  �   �
  �"      ��  �   �
  L#      �  �   �
  �#      $�  �   �
  4$      8�  �   �
  �$      L�  �   �
  ,%      `�  �   �
  �%      t�  �   �
  $&      ��  �   �
  �&      ��  �   �
  '      ��  �   �
  �'      ď  �   �
  (      ؏  �   �
  �(      �  �   �
  )       �  �   �
  �)      �  �   �
  *      (�  �   �
  �*      <�  �   �
  �*          �   �
  x+      l�      l�  �      �+      4   �����+                ��                      ��                    �                  �V                         |�  �  �     @,       �  �     �,      4�  �     8-      H�  �     �-      \�  �      .      p�  �     �.      ��  �      /      ��  �   !  D/      ��  �   "  �/      ��  �   #  �/      ԑ  �   $  00      �  �   %  �0      ��  �   &  1      �  �   '  �1      $�  �   )  2      8�  �   *  |2      L�  �   +  �2      `�  �   ,  l3      t�  �   -  �3      ��  �   .  $4      ��  �   0  �4      ��  �   1  5      Ē  �   2  �5      ؒ  �   3  �5      �  �   4  �5       �  �   5  t6      �  �   6  �6      (�  �   7  �6      <�  �   8  (7      P�  �   9  d7      d�  �   :  �7      x�  �   ;  �7      ��  �   <  8      ��  �   >  �8      ��  �   ?  �8      ȓ  �   @  9      ܓ  �   A  @9      �  �   B  |9      �  �   C  �9      �  �   D  �9      ,�  �   E  0:      @�  �   F  �:      T�  �   G  ;      h�  �   H  �;      |�  �   I   <      ��  �   J  |<      ��  �   K  �<      ��  �   L  t=      ̔  �   M  �=      ��  �   N  l>      ��  �   O  �>      �  �   P  $?      �  �   Q  �?      0�  �   R  �?      D�  �   S  @      X�  �   T  T@          �   U  �@      ĕ  $  �  ��  ���                       0A     
                    � ߱        \�      ��  �      <A      4   ����<A      /     �     ,�                          3   ����LA            L�                      3   ����lA  ��      x�  ��  ��  �A      4   �����A  	              �                      ��             	       �                  �                         ��  �  �     �A      p�  $    D�  ���                       B     
                    � ߱        ��  �     4B      ܗ  $     ��  ���                       \B  @         HB              � ߱        ��  $  !  �  ���                       �B                         � ߱        $C     
                �C                     �D  @        
 �D              � ߱        (�  V   +  4�  ���                        �D                     0E                     lE                         � ߱        ��  $  G  Ę  ���                       ,F     
                �F                     �G  @        
 �G              � ߱        H�  V   Y  T�  ���                        H     
                �H                     �I  @        
 �I              � ߱            V   ~  �  ���                        
              ��                      ��             
     �  9                  ��                       �  t�  �I     
                XJ                     �K  @        
 hK          L  @        
 �K          lL  @        
 ,L          �L  @        
 �L              � ߱            V   �  �  ���                        adm-clone-props \�  ԛ              �     7     `                          \  Y                     start-super-proc    �  @�  �           �     8                                  z                     H�    Q  ̜  ܜ      XP      4   ����XP      /   R  �     �                          3   ����hP            8�                      3   �����P  ��  $  l  t�  ���                       �P                         � ߱        \�    |  ��  8�  ؞  �P      4   �����P                ��                      ��                  }  �                  4~                       }  ̝  �P                     �P                        Q       !       !           � ߱            $  ~  H�  ���                             �  ��  0�      Q      4   ����Q  8Q                         � ߱            $  �  �  ���                       X�    �  x�  ��  ��  LQ      4   ����LQ      $  �  ��  ���                       lQ       !       !           � ߱            �   �  �Q      �Q     
                <R                     �S  @        
 LS              � ߱        ��  V   �  ��  ���                        ��  �   �  �S      0�    q  ��  Ġ      �S      4   �����S      /   r  �      �                          3   �����S             �                      3   ����T  �  $  v  \�  ���                       $T       "       "           � ߱        PT     
                �T                     V  @        
 �U              � ߱        �  V   �  ��  ���                        ��    �  4�  ��      (V      4   ����(V                ��                      ��                  �  �                  �t                       �  D�      g   �  آ         v���                           ��          p�  X�      ��                  �      ��              �&                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ̣     ܣ  PV                      3   ����8V  �     
   ��                      3   ����\V         
   ,�                      3   ����dV    ��                              ��        �                  ����                                        �              9      <�                      g                                �  g     �          v�	��                           إ          ��  ��      ��                      ��              L)                    O   ����    e�          O   ����    R�          O   ����    ��          /    �     �  �V                      3   ����lV            4�                      3   �����V    ��                              ��        �                  ����                                        $�              :      D�                      g                               �  g     �          v�	��                           �          ��  ��      ��                      ȧ              �)                    O   ����    e�          O   ����    R�          O   ����    ��          /    �     �  �V                      3   �����V            <�                      3   �����V    ��                              ��        �                  ����                                        ,�              ;      L�                      g                               h�      $�  ��      �V      4   �����V                ��                      ��                    >                  ��                         4�  �  /      ܩ     �                          3   �����V            �                      3   ����W  �  /  "  H�     X�  XW                      3   ����8W  ��     
   x�                      3   ����`W  ��        ��                      3   ����hW  �        ت                      3   ����|W            �                      3   �����W  @�    *  4�  D�      �W      4   �����W      /  0  p�     ��  LX                      3   ����,X  ��     
   ��                      3   ����TX  �        Ы                      3   ����\X  �         �                      3   ����pX            0�                      3   �����X        6  \�  l�      �X      4   �����X      /  9  ��     ��  Y                      3   �����X  ج     
   Ȭ                      3   ����Y  �        ��                      3   ����Y  8�        (�                      3   ����,Y            X�                      3   ����HY  (�    B  ��   �      lY      4   ����lY                �                      ��                  C  F                  �}                       C  ��      g   D  (�         v�̯        |Y                  �          ��  ��      ��                  E      خ              �}                    O   ����    e�          O   ����    R�          O   ����    ��          /  E  �     ,�  �Y                      3   �����Y  \�     
   L�                      3   �����Y         
   |�                      3   �����Y    ��                            ����                                        <�              <      ��                      g                               ��     J  �Y                                     �Y     
                LZ                     �[  @        
 \[              � ߱        P�  V   �  \�  ���                        �[     
                ,\                     |]  @        
 <]              � ߱        |�  V   �  �  ���                         �      ��  ��      �]      4   �����]      $     Ա  ���                       �]  @         �]              � ߱        Գ  g   (  �         v�x�        ^  v�x�        ^                  ��          Ĳ  ��      ��                  )  .  ܲ              �%]                    O   ����    e�          O   ����    R�          O   ����    ��            -  �   �      ^      4   ����^      O  -  ������  0^    ��                            ����                                        @�              =      8�                      g                               ��  g   5  �         v6$�         D^                  ��          ��  l�      ��                  6  ;  ��              $&]                    O   ����    e�          O   ����    R�          O   ����    ��      ̴    9  P^  }          O  :  ������  d^    ��                            ����                                         �              >      �                      g                               �  g   C  ��         v"��                           ̶          0�  �      ��                 D  `  H�              `�_                    O   ����    e�          O   ����    R�          O   ����    ��        ܶ      4�  �          ��  ��      ��       0         H  J  ̷              ��_      �^     ��     H  `�      $  H  �  ���                       x^                         � ߱        ��  $  H  `�  ���                       �^                         � ߱            4   �����^      O   ����  e�          O   ����  R�          O   ����  ��          �   I  _      P�  $  L  $�  ���                       ,_       
       
           � ߱        ��  $  M  |�  ���                       8_                         � ߱         �  $  O  Ը  ���                       D_                         � ߱        X�  $  T  ,�  ���                       X_                         � ߱        ��  $  V  ��  ���                       �_       	       	           � ߱        �  $  W  ܹ  ���                       �_                         � ߱        `�  $  X  4�  ���                       �_                         � ߱        ��  /   \  ��                                 3   �����_      /   ^  Ⱥ                                 3   �����_                �                                           ��                              ��        �                   ��                            ����                            T�          ��  غ         ?     �                      g   �                          ��  g   h  �         v"P�                           м          ��  ��      ��H�               i  �  ��              �                    O   ����    e�          O   ����    R�          O   ����    ��       �    j  �  h�      �_      4   �����_                x�                      ��                  j  m                  `�                       j  ��  ��  	   k  ��                                          3   ����`      O  l  ������  `                            � ߱        ,�  $   s  Խ   �                       ��  $  u  X�  ���                       (`                         � ߱        ܾ  $  v  ��  ���                       4`                         � ߱        4�  $   x  �  ���                       \`  @         H`              � ߱        �  A  z        ��   ��         ��  �`                                         x`   �`   �`                 ��  �           �`  �`  �`           �`  �`  �`         �            ��   п          ~  $�  ��  X�   a      4   ���� a                ��                      ��                  ~  �                  (�                       ~  4�                            � ߱        �  $   �  ��   �                       `�  $   �  4�  ���                       <a  @         (a              � ߱            /   �  ��     ��                          3   ����\a  ��        ��                      3   �����a  ��        ��                      3   �����a  ,�        �                      3   �����a  \�        L�                      3   �����a  ��        |�                      3   �����a  ��        ��                      3   �����a            ��                      3   �����a                h�                      ��                  �  �                  �l�                       �  ��      	   �  ��                                          3   �����a                ��                                           ��                              ��        �                  ����                                  ��          �  ��         @     ��                      g   ��                          ��  g   �  ��         v d�        	                   ��          \�  D�      ��\�               �  �  t�              �l�                    O   ����    e�          O   ����    R�          O   ����    ��                                � ߱        ��  $   �  ��   �                       <�  $  �  �  ���                       �a                         � ߱        ��  $  �  h�  ���                        b                         � ߱        ��  $   �  ��  ���                       (b  @         b              � ߱        ��  A  �        T�   ��         <�  �b                                         Db   Xb   db                 ��  ��           pb  �b  �b           xb  �b  �b         �            p�   ��          �  ��  X�      �b      4   �����b                h�                      ��                  �  �                  1�                       �  ��      $   �  ��  ���                       c  @         �b              � ߱                      ��                                           ��                              ��        �                  ����                                  P�          ��  ��         A     ��                      g   ��                          ��  g   �  ��         v���                            ��          p�  X�      ��                  �  �  ��              �p�                    O   ����    e�          O   ����    R�          O   ����    ��      ��  $  �  ��  ���                       (c                         � ߱        P�  $  �  $�  ���                       <c                         � ߱        ��  $  �  |�  ���                       pc       
       
           � ߱            /   �  ��                                 3   ����|c                �                                           ��                              ��        �                  ����                            d�          ��  ��         B      �                      g   �                                �  ��  t�      �c      4   �����c                ��                      ��                  �                    암                       �  �  �c  @                     �c  @         �c          �c  @         �c              � ߱        �  $   �  ��  ���                       �  g   �  ,�         vn��      }                      ��          ��  ��      ��                  �  �  ��              ,��                    O   ����    e�          O   ����    R�          O   ����    ��      0�  /  �   �                                 3   ����d        �  L�  \�      $d      4   ����$d      O  �  ������  Xd    ��                            ����                                        @�              C      t�                      g                               ��  g   �  (�         v!��         ld                  �          ��  ��      ��                  �  �  ��              ���                    O   ����    e�          O   ����    R�          O   ����    ��      xd  @                         � ߱            $  �  ��  ���                         ��                            ����                                        <�              D      H�                      g                                �  /   �  �                                 3   �����d          <�  ��      �d      4   �����d                4�                      ��                                      �͔                         L�                t�          \�  D�      ��                                      Δ                         ��      O       ��          O       ��      ��  /   	  ��                                 3   �����d        
  ��  ��      �d      4   �����d      k     ��              }       n        �   adm-create-objects  ��  �                      E      �                               �                     carga-impresoras    $�  ��              �     F     �                          �  /                      disable_UI  ��  ��                      G      <                              @   
                   enable_UI   ��  X�                      H      `                              K   	                   enumerateprinters   d�  ��          �  0    I     �                          �  n                      exitObject  ��  0�                      J      �                               �   
                   show-attributos <�  ��                      K      l                              �                       ����   �   # �         �BOL        ���  �             8   ����       8   ����             t�  ��      toggleData  ,INPUT plEnabled LOGICAL    d�  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  �  �      returnFocus ,INPUT hTarget HANDLE   ��  <�  P�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ,�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE |�  ��  ��      removeAllLinks  ,   ��  �   �      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE  �  x�  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    h�  �  �      hideObject  ,   ��  $�  <�      editInstanceProperties  ,   �  P�  `�      displayLinks    ,   @�  t�  ��      createControls  ,   d�  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  �  �      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  t�  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER d�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  4�  D�      unbindServer    ,INPUT pcMode CHARACTER $�  l�  ��      startServerObject   ,   \�  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  ��  ��      restartServerObject ,   ��  ��  �      initializeServerObject  ,   ��  $�  8�      disconnectObject    ,   �  L�  `�      destroyServerObject ,   <�  t�  ��      bindServer  ,   d�  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  ��  ��      enableObject    ,   ��  ��  �      disableObject   ,   ��  �  $�      applyLayout ,   �  8�  D�      viewPage    ,INPUT piPageNum INTEGER    (�  p�  |�      viewObject  ,   `�  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  ��  �      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  L�  X�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  <�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  �  (�      initializeVisualContainer   ,   ��  <�  P�      initializeObject    ,   ,�  d�  p�      hidePage    ,INPUT piPageNum INTEGER    T�  ��  ��      destroyObject   ,   ��  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  ��  �      createObjects   ,   ��  �  ,�      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE �  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  ��  ��      changePage  ,   ��  ��  �      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 ^%     adecomm/as-utils.w 
"   
   �    }        �
"     
   %              %                  �     }        �G� �   �G%              � �     %        %       	 %        %       %        %       %               %               %               %              %              %              %               %              
�        
"   
 ^
�    
"   
 ^
"   
 �    �             �            
"   
   �        X         �     }        �%              
"   
 ^
"   
 �    �        �     �        �    
"   
   �        �         �     }        �%              � 
"    
 �%              � �  �         X      $              
�    �    �     
"   
                       
�            �    �
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 ��           �    1� +  
 �� 6   �%               o%   o           � ;    �
"   
 ��           X    1� <   �� 6   �%               o%   o           � J   �
"   
 ��           �    1� Q  
 �� 6   �%               o%   o           � \   �
"   
 ��           @    1� h   �� 6   �%               o%   o           � v   �
"   
 ��           �    1� }   �� 6   �%               o%   o           � �   �
"   
 ��           (    1� �   �� �   �%               o%   o           %               
"   
 ��          �    1� �   �� �     
"   
 ��           �    1� �   �� 6   �%               o%   o           � �  e �
"   
 ��           T    1� G   �� 6   �%               o%   o           � V  [ �
"   
 ��           �    1� �   �� �   �%               o%   o           %               
"   
 ��           D	    1� �   �� �   �%               o%   o           %               
"   
 ��           �	    1� �   �� �   �%               o%   o           %              
"   
 ��          <
    1� �   �� �     
"   
 ��           x
    1� �  
 �� �   �%               o%   o           %               
"   
 ��           �
    1� �   �� 6   �%               o%   o           � ;    �
"   
 ��          h    1�    �� �     
"   
 ��           �    1�    �� 6   �%               o%   o           � )  t �
"   
 ��              1� �  
 �� �     
"   
 ��           T    1� �   �� 6   �%               o%   o           � �  � �
"   
 ��           �    1� G   �� 6   �%               o%   o           � ;    �
"   
 ��           <    1� ^  
 �� i   �%               o%   o           %               
"   
 �           �    1� m   � �   �%               o%   o           %               
"   
 ]�           4    1� u   ]� 6   �%               o%   o           � ;    
"   
 ]�           �    1� �   ]� 6   �%               o%   o           o%   o           
"   
 �           $    1� �  
 � 6   �%               o%   o           � ;    
"   
 ]�           �    1� �   ]� �  	 �%               o%   o           � �  / 
"   
 ��              1� �   �� �  	   
"   
 �           H    1� �   � �  	 �o%   o           o%   o           � ;    
"   
 ��          �    1�    �� �  	   
"   
 �           �    1�     � �  	 �o%   o           o%   o           � ;    
"   
 ��          l    1� 0   �� �     
"   
 ��          �    1� >   �� �  	   
"   
 ��          �    1� K   �� �  	   
"   
 ��               1� X   �� �  	   
"   
 �           \    1� f   � �   �o%   o           o%   o           %              
"   
 ��          �    1� w   �� �  	   
"   
 ��              1� �  
 �� �     
"   
 ��          P    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��              1� �   �� �  	   
"   
 ��          @    1� �  	 �� �  	   
"   
 ��          |    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ]�           �    1�    ]� 6   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
 �(�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�       �    �� '     p�               �L
�    %              � 8      �    � $         � .          
�    � H     
"   
 �� @  , 
�       �    �� Q  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� K  
 � 6   �%               o%   o           � ;    
"   
 �               1� V  
 � 6   �%               o%   o           o%   o           
"   
 �           �    1� a   � �   �%               o%   o           o%   o           
"   
 ]�               1� j   ]� �   �%               o%   o           %               
"   
 �           �    1� y   � �   �%               o%   o           %               
"   
 ^�                1� �   ^� 6   �%               o%   o           � ;    
"   
 �           t    1� �   � �   �%               o%   o           %              
"   
 �           �    1� �   � �   �%               o%   o           o%   o           
"   
 �           l    1� �   � 6   �%               o%   o           o%   o           
"   
 �           �    1� �  	 � 6   �%               o%   o           � ;    
"   
 �           \    1� �   � 6   �%               o%   o           o%   o           
"   
 �           �    1� �   � 6   �%               o%   o           o%   o           
"   
 �           T    1� �   � �   �%               o%   o           %               
"   
 �           �    1� �   � �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1�    � �  	 �%               o%   o           � ;    
"   
 �               1�    � �  	 �%               o%   o           � ;    
"   
 �           �    1�    � �   �%               o%   o           %               
"   
 ^�                1� +   ^� �  	 �%               o%   o           � ;    
"   
 �           x     1� :   � �  	 �%               o%   o           � ;    ^
"   
 �           �     1� H   � �   �%               o%   o           %               
"   
 ]�           h!    1� V   ]� �  	 �%               o%   o           � ;    
"   
 �           �!    1� e   � �  	 �%               o%   o           � ;    ]
"   
 �           P"    1� t   � �  	 �%               o%   o           � ;    
"   
 �           �"    1� �   � �  	 �%               o%   o           o%   o           
"   
 �           @#    1� �   � �  	 �%               o%   o           � ;    
"   
 ^�           �#    1� �   ^� �  	 �%               o%   o           � ;    
"   
 �           ($    1� �  	 � �   �%               o%   o           %               
"   
 �           �$    1� �   � �   �%               o%   o           %               
"   
 �            %    1� �   � �   �%               o%   o           o%   o           
"   
 ]�           �%    1� �   ]� �   �%               o%   o           o%   o           
"   
 �           &    1� �   � �   �%               o%   o           %               
"   
 �           �&    1� �   � �   �%               o%   o           %               
"   
 �           '    1�     � �   �%               o%   o           %               
"   
 ^�           �'    1�    ^� !   �%               o%   o           %       
       
"   
 ^�           (    1� )   ^� !   �%               o%   o           o%   o           
"   
 �           �(    1� 5   � !   �%               o%   o           %              
"   
 �            )    1� A   � !   �%               o%   o           o%   o           
"   
 �           |)    1� M   � !   �%               o%   o           %              
"   
 �           �)    1� Z   � !   �%               o%   o           o%   o           
"   
 �           t*    1� g   � !   �%               o%   o           %              
"   
 �           �*    1� o   � !   �%               o%   o           o%   o           
"   
 ^�           l+    1� w   ^� �  	 �%               o%   o           � ;    P �L 
�H T   %              �     }        �GG %              
"   
 �           4,    1� �   � i   �%               o%   o           %               
"   
 �           �,    1� �   � i   �%               o%   o           o%   o           
"   
 �           ,-    1� �   � 6   �%               o%   o           � ;    
"   
 �           �-    1� �   � 6   �%               o%   o           � �  - 
"   
 �           .    1� �   � 6   �%               o%   o           � ;    
"   
 �           �.    1�    � 6   �%               o%   o           � )   
"   
 ��          �.    1� G   �� �     
"   
 �           8/    1� X   � 6   �%               o%   o           � ;    
"   
 ��          �/    1� d  
 �� �     
"   
 ��          �/    1� o   �� �     
"   
 �           $0    1� |   � �  	 �%               o%   o           � ;    
"   
 �           �0    1� �   � 6   �%               o%   o           � ;    
"   
 �           1    1� �   � �   �%               o%   o           o%   o           
"   
 �           �1    1� �   � 6   �%               o%   o           � �  ! ]
"   
 �           �1    1� �   � 6   �%               o%   o           � ;    
"   
 ^�           p2    1� �   ^� 6   �%               o%   o           � �   
"   
 ^�           �2    1�   	 ^� i   �%               o%   o           o%   o           
"   
 �           `3    1�    � �   �%               o%   o           %               
"   
 ��          �3    1�    �� �     
"   
 �           4    1� +   � 6   �%               o%   o           � ?   
"   
 ]�           �4    1� N   ]� �  	 �%               o%   o           � ;    
"   
 �            5    1� [   � �  	 �%               o%   o           � ;    ]
"   
 ��          t5    1� k   �� �     
"   
 ��          �5    1� }   �� �  	   
"   
 ^�           �5    1� �   ^� �   �o%   o           o%   o           %               
"   
 ��          h6    1� �   �� �     
"   
 ��          �6    1� �   �� �  	   
"   
 ��          �6    1� �   �� �  	   
"   
 ��          7    1� �   �� �  	   
"   
 ��          X7    1� �   �� �  	   
"   
 ��          �7    1�    �� �  	   
"   
 ��          �7    1�    �� �     
"   
 �           8    1� #   � 6   �%               o%   o           � :  4 
"   
 ��          �8    1� o   �� �     
"   
 ��          �8    1� |   �� �     
"   
 ��          �8    1� �   �� �     
"   
 ��          49    1� �   �� �  	   
"   
 ��          p9    1� �   �� �  	   
"   
 ��          �9    1� �   �� �  	   
"   
 ��          �9    1� �   �� �     
"   
 �           $:    1� �   � �  	 �%               o%   o           � ;    
"   
 �           �:    1� �   � �  	 �%               o%   o           � ;    
"   
 �           ;    1� �   � �  	 �%               o%   o           � ;    
"   
 �           �;    1�    � �  	 �%               o%   o           � ;    
"   
 �           �;    1� "   � �   �%               o%   o           %               
"   
 �           p<    1� 0   � �   �%               o%   o           o%   o           
"   
 ]�           �<    1� B   ]� �   �%               o%   o           %               
"   
 �           h=    1� R   � �   �%               o%   o           %               
"   
 �           �=    1� ^   � �   �%               o%   o           o%   o           
"   
 �           `>    1� y   � �   �%               o%   o           %               
"   
 ��          �>    1� �   �� �  	   
"   
 �           ?    1� �   � �   �%               o%   o           %              
"   
 ��          �?    1� �   �� �  	   
"   
 ��          �?    1� �   �� �  	   
"   
 ��          @    1� �  
 �� �  	   
"   
 �           H@    1� �   � �  	 �%               o%   o           � "   
"   
 �           �@    1� �   � �  	 �%               o%   o           � ;    
"   
    "    �%     start-super-proc ��%     adm2/smart.p v�P �L 
�H T   %              �     }        �GG %              
"   
   �       �A    6�      
"   
   
�        B    8
"   
   �        (B    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        pC    ��    � P   �        |C    �@    
� @  , 
�       �C    �� '   �p�               �L
�    %              � 8      �C    � $         � .          
�    � H   �
"   
 �p� @  , 
�       �D    �� �   �p�               �L"    , �   �    �    ��     }        �A      |    "      �    %              (<   \ (    |    �     }        �A�    �A"        "    �"      < "    �"    (    |    �     }        �A�    �A"    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        xF    ��    � P   �        �F    �@    
� @  , 
�       �F    �� '   �p�               �L
�    %              � 8      �F    � $         � .          
�    � H   �
"   
 �p� @  , 
�       �G    �� +  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        PH    ��    � P   �        \H    �@    
� @  , 
�       hH    �� '   �p�               �L
�    %              � 8      tH    � $         � .          
�    � H   �
"   
 �p� @  , 
�       �I    �� �   �p�               �L
"   
 , 
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
   (�  L ( l       �        (J    ��    � P   �        4J    �@    
� @  , 
�       @J    �� '     p�               �L
�    %              � 8      LJ    � $         � .          
�    � H     
"   
 �p� @  , 
�       \K    �� Q  
 �p�               �L%     SmartWindow 
"   
   p� @  , 
�       �K    �� h     p�               �L%      WINDOW  
"   
  p� @  , 
�        L    ��      p�               �L%               
"   
  p� @  , 
�       �L    �� �    p�               �L(        � ;      � ;      � ;      �     }        �A
�H T   %              �     }        �GG %              
"   
  (   � 
"   
 �    �        `M    ��    �
"   
   � 8      �M    � $         � .          
�    � H   �
"   
   �        N    �
"   
   �       $N    /
"   
   
"   
   �       PN    6�      
"   
   
�        |N    8
"   
   �        �N    �
"   
   �       �N    �
"   
   p�    � H   
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �O    �A"    �A
"   
   
�        �O    �@ � 
"   
 "      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p X�    � �     
�    �     }        �%               %      Server  - �     }        �    "    � ;    �%                   "     � ;    �%      NONE    p�,  8         $     "     ]        � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        R    ��    � P   �        R    �@    
� @  , 
�       $R    �� '   �p�               �L
�    %              � 8      0R    � $         � .          
�    � H   �
"   
 �p� @  , 
�       @S    �� �   �p�               �L"  !  , p�,  8         $     "    ]        � �   �
�     "    �%     start-super-proc ��%     adm2/visual.p ��   �      �      �   F   
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �T    ��    � P   �        �T    �@    
� @  , 
�       �T    �� '   �p�               �L
�    %              � 8      �T    � $         � .          
�    � H   �
"   
 �p� @  , 
�       �U    �� V   �p�               �L"  "  , � 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP v�%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %      initializeDataObjects 0 0   A    �    � �   
�    � �   �A    �    � �     
�    � �   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %     buildDataRequest ent0 A    �    � �   �
�    � �   %     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
"    
 �
"   
 �%     contextHelp 
"    
   
�    
�    %               
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 (�  L ( l       �        Z    ��    � P   �        (Z    �@    
� @  , 
�       4Z    �� '   �p�               �L
�    %              � 8      @Z    � $         � .   �     
�    � H   �
"   
 �p� @  , 
�       P[    �� k   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        �[    ��    � P   �        \    �@    
� @  , 
�       \    �� '   �p�               �L
�    %              � 8       \    � $         � .   �     
�    � H   �
"   
 �p� @  , 
�       0]    �� "   �p�               �L%              (        �     }        �G� �   �G� 
"   
 �
"   
   �        �]    �%              
"   
 �
"   
 ��     }        �%               
"   
 �%      CLOSE   %                   %              %                   "      %                  "      �             
&�             
&�            "      �       �       %                    "      %              �       �       %              %     enumeratePrinters �%     show-attributos     "  
  �     ��      %               "      �       	     B�       
     B     %         		   %              "      "      &    &    &    &    &    &    0        %              %              %              *    �       
     B    "    B� U   B%, !    sunat/r-impresion-doc-electronico �"  1  �"    �"    �� c   �"    �%              "  
    � e     "      �       	     B�       
     B     %         		   %              "      "      &    &    &    &    &    &    0        %              %              %              *    �       
     B    "    B� U   B�             BT   %              "      � �   "      %     show-attributos � 
"   
 �
"   
 
"   
 ��        �c    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � �  	   %               
"   
 �
�    %     createObjects    �     }        �%     initializeObject � �     }        ��    "      %       T           %               %                   "      %               (   "          "      %                  "      %              � @   "        (   � "          "    �%       T       � (   "      �    "      %              �    "      %              � (   "      �    "      %              �    "      %              � (   "      �    "      %       	       �    "      %              � (   "      �    "      %              �    "      %              � (   "      �    "      %              �    "      %              � (   "      �    "      %              �    "      %              � (   "      �    "      %              �    "      %              � 4                      "      � +    �"    (        �     }        �G� �   �G� 
"   
 �
"   
   �     }        �
�    
"   
 �"    "    �"      "      "      "      "      "      "      "      
"   
 
"   
   '�     }        �    %              %                   "      %                   "      �    '"   '   � l      �    '"   '   � l     T   "      '"   '   � l      �            "      %      CLOSE   %               �            B�     }        �G�            B     �     }        ��            B     �     }        ��            B�     }        ��            B�     }        �                �           �   l       ��                 �  �  �               �                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       M     
                    � ߱              �  (  �      lM      4   ����lM                �                      ��                  �  �                  ĉ                       �  8  �  �  �  �M            �  �  `      N      4   ����N                p                      ��                  �  �                  (�                       �  �  �  o   �      ,                                 �  �   �  0N      �  �   �  \N      $  $  �  �  ���                       �N     
                    � ߱        8  �   �  �N      L  �   �  �N      `  �   �  �N          $   �  �  ���                       O  @         O              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  2  �               �                    O   ����    e�          O   ����    R�          O   ����    ��      i                      �          �  $      ���                       lO     
                    � ߱                  �  �                      ��                                       �V                       4      4   �����O      $    �  ���                       �O     
                    � ߱        �      4  D      �O      4   �����O      /    p                               3   ���� P  �  �   #  P          O   0  ��  ��  DP                               , �                          
                               �      ��                            ����                                                        �   l       ��                    "  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                 (  g  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      x  V   <  �   ���                        �d                         � ߱          �      �  H                      ��        0         =  d                  ��      �e            =        $  =  �  ���                       e                         � ߱        8  $  =    ���                       De                         � ߱            4   ����le  �  V   >  t  ���                        �e                         � ߱        �  V   A  �  ���                        $f                         � ߱        P  $ B  $  ���                       `f                         � ߱        �  V   D  |  ���                        �f                         � ߱           $ E  �  ���                       �f                         � ߱        X  V   G  ,  ���                        �f                         � ߱        �  $ H  �  ���                       (g                         � ߱          V   J  �  ���                        Pg                         � ߱        `  $ K  4  ���                       �g                         � ߱        �  V   M  �  ���                        �g                         � ߱          $ N  �  ���                       �g       	       	           � ߱        h  V   P  <  ���                        h                         � ߱        �  $ Q  �  ���                       Th       
       
           � ߱          V   S  �  ���                        |h                         � ߱        p  $ T  D  ���                       �h                         � ߱            �   V  �h                   �          X  �  , � �                                                                                                                                                       ,   <   L   \   l   |   �   �   �   �       ,   <   L   \   l   |   �   �   �   �    ��             ��                              ��        �                  ����                                            �           �   l       ��                  m  z  �               d��                    O   ����    e�          O   ����    R�          O   ����    ��           w  �   �       (i      4   ����(i      n   x     �          hi        y    ,      ti      4   ����ti      �   y  �i    ��                            ����                                            d          �   l       ��                  �  �  �               �Õ                    O   ����    e�          O   ����    R�          O   ����    ��      �i  �           �i  �          �i  �          �i  �          �i  �          �i  �          �i  �          �i  �          �i  � 	         j  � 
             � ߱          Z   �  �    �        �i                  �              �               �              �              �              � 	             �              � ߱        <  h   �  �   �        j                  
   �  �� X              j    ��                              ��        �                  ����                                            �           �   l       ��                 �  �  �               (@�                    O   ����    e�          O   ����    R�          O   ����    ��      x  V   �  �   ���                        ,j     '                    � ߱          �      �  �          `  H      ��       0         �  �  x              �A�      �j            �        $  �  �  ���                       @j                         � ߱        8  $  �    ���                       pj                         � ߱            4   �����j      O   ����  e�          O   ����  R�          O   ����  ��      �  $  �  �  ���                       �j                         � ߱            �   �  k                    d                                               �          x  �    h                '                        �      ��                             ��                              ��        �                  ����                                            �           �   l       ��                  �  �  �               �t�                    O   ����    e�          O   ����    R�          O   ����    ��      �     �  8k  }          O   �  ��  ��  Lk    ��                            ����                                            �           �   l       ��                  �    �               �w�                    O   ����    e�          O   ����    R�          O   ����    ��        $     �   ���                       tk  @         `k              � ߱        d  $     8  ���                       �k  @         �k              � ߱        �  $     �  ���                       �k  @         �k              � ߱          $   	  �  ���                       �k  @         �k              � ߱            $   
  @  ���                       $l  @         l              � ߱          ��                              ��        �                  ����                               X   d d     �   ��G%�H%  � �                                               �                                                                         d     D                                                                 p  ,� �#                                                        �     �             D                P   �3�d                                                           �   G   
 X  �34d                                                        �     �      P   �3d                                                           �   G   
 X  �3yd                                                        2     �      P   ���d                                                           �   G   
 X  ���	d                                                        k     �      P   ���d                                                           �   G   
 X  ��yd                                                        A     �      P   @�d                                                           �   G   
 X  @�	d                                             
           P     �      \  �f�p                                 �                 �                 @      h  �`�M                                                        �     �     �                P   �l�d                                                           �   G     �  �l�l                                                             �  -           "           "  &  .       P   @d                                                           �   G   
 X  @4d 	                                                       �     �      P   ��d                                                           �   G   
 X  ��d 
        d   x                                          {     �  
    \   �p                                                  !                @      P �@� >                                                        �       H  ,'�"�                                 �                     D                                                                    TXS appSrvUtils ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST mpPrinterEnum iFlags cName iLevel ipcbNeeded ipcReturned iResult xPrinterTS xPrinterName xPrinterPort wWin BUTTON-1 BUTTON-2 cboTipoDoc BOL BOLETA BOl FACTURA FAC txt-print-name txt-print-port txt-printer-control-handle txt-printer-hdc txtImpte txtnrodoc txtOS RECT-1 xselectionlist chbticket fMain x(8) X(256) yes/no X(45) ->>,>>9.99 ... GUI Lista de Impresoras DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   RECT-1 xselectionlist BUTTON-1 chbticket cboTipoDoc txtnrodoc BUTTON-2 CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE x-sec  Seleccione Impresor por favor x-coddoc x-nrodoc CcbCDocu ->,>>>,>>9.99 O Documento NO EXISTE x-filer1 x-filer2 x-filer3 | iStartPage ADM-ERROR ADM-CREATE-OBJECTS iCounter mpPrinterInfo2 mBuffer cServerName cPrinterName cShareName cPortName cDriverName cComment cLocation  |  CARGA-IMPRESORAS DISABLE_UI ENABLE_UI x-impresoras x-printer , ENUMERATEPRINTERS EXITOBJECT SHOW-ATTRIBUTOS S.O. Print Name HDC Port Control Handler Impresoras Imprimir en Ticketera Tipo Documento Numero Importe Imprimir llave01 �  �$      P+      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
 pcProcName  �   ��      �         pcProcName      ��              
 pcProcName      ��      $        piPageNum       ��      H        piPageNum       ��      l        pcPageList      ��      �        pcProc  �  ��      �        pcLinkName      ��      �        pcLinkName    ��      �       
 phTarget        ��              phTarget        ��      @        piPageNum       ��      d        pcValue     ��      �        piPageNum       ��      �        pcAction        ��      �       
 phAppService        ��      �        pcMode     ��             
 phSource    D  ��      8        phSource        ��      \       
 phSource    �  ��      �        pcText  �  ��      �        pcText      ��      �        pcText  �  ��      �       
 phObject      ��             
 phObject        ��      (        phObject        ��      L        pcField     ��      l        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller    �  ��      �        phCaller        ��      �        phCaller    (  ��               pcMod   H  ��      @        pcMod       ��      `       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    �  ��      �        pdRow       ��              pdRow       ��      ,       
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   �	  �	  �	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc                #  0  2  H  �     9                                   �  �  	     :                                       �  L	     ;                                       	  �	     <                                   E  T	  �	     =                                   -  .  �	  �	     >                                   9  :  ;            
     x-sec   �	  H
     ?   �	                              H  I  J  L  M  O  T  V  W  X  \  ^  `  �
        �
     x-coddoc              �
     x-nrodoc    
  �
     @   |
                              j  k  l  m  s  u  v  x  z  ~  �  �  �  �  �  �  �  �  T        H     x-coddoc              h     x-nrodoc    �
  �  	   A   4                              �  �  �  �  �  �  �  �  �  �        �     x-filer1            �     x-filer2                   x-filer3    t  X     B   �                              �  �  �  �  �  (  �     C                                   �  �  �  �  l  �     D                                   �  �  �  (     E                                 adm-create-objects  "  L        @     iCounter    p        `     mpPrinterInfo2  �        �     mBuffer �        �     cServerName �        �     cPrinterName    �        �     cShareName               cPortName   0     	   $     cDriverName P     
   D     cComment              d     cLocation   �  �     F   ,          �                  carga-impresoras    <  =  >  A  B  D  E  G  H  J  K  M  N  P  Q  S  T  V  d  g  p  @     G               4                  disable_UI  w  x  y  z    �     H               �                  enable_UI   �  �  �  �  �        �  '   x-impresoras    �       �     x-sec            �     x-printer   P  @     I   �          ,                  enumerateprinters   �  �  �  �  �  �  �  �     J               �                  exitObject  �  �  �  X  �     K               �                  show-attributos       	  
    �  H       (      ,                      H          <  
   appSrvUtils l       \     mpPrinterEnum   �       �     iFlags  �       �     cName   �       �     iLevel  �       �     ipcbNeeded          �     ipcReturned             iResult <    	   0     xPrinterTS  `    
   P     xPrinterName    �       t     xPrinterPort    �       �  
   wWin    �       �     cboTipoDoc  �       �     txt-print-name         �     txt-print-port  8            txt-printer-control-handle  \       L     txt-printer-hdc |       p     txtImpte    �       �     txtnrodoc   �       �     txtOS   �       �     xselectionlist  �       �     chbticket   $          
   gshAstraAppserver   L        8  
   gshSessionManager   p        `  
   gshRIManager    �        �  
   gshSecurityManager  �        �  
   gshProfileManager   �        �  
   gshRepositoryManager      	 	        
   gshTranslationManager   <  
 
     ,  
   gshWebManager   `        P     gscSessionId    �        t     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager                gsdTempUniqueID 4        (     gsdUserObj  \        H     gsdRenderTypeObj    �        p     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf          �     glADMLoadFromRepos  (             glADMOk H       <  
   ghContainer h       \     cObjectName �       |     iStart  �       �     cAppService �        �     cASDivision �    !   �     cServerOperatingMode        "        cFields       #         iStartPage           <  CcbCDocu             7   �  �  �  �  �  �  �  �  �  �  {  |  }  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  r	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  n
  y
  z
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
                       !  "  #  $  %  &  '  )  *  +  ,  -  .  0  1  2  3  4  5  6  7  8  9  :  ;  <  >  ?  @  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  �  �                  !  +  G  Y  ~  �  �  �  9  Q  R  l  |  }  ~  �  �  �  �  �  �  �  �  q  r  v  �  �  �  �  �             "  *  0  6  9  >  B  C  D  F  J  �  �      (  5  C  h  �  �  �  �  �  �  �      	  
              H� $ C:\Progress\OpenEdge\src\adm2\windowmn.i d  f!  C:\Progress\OpenEdge\src\adm2\containr.i �  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\visual.i     # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  D  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    �  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �  I�  C:\Progress\OpenEdge\src\adm2\smart.i       Ds   C:\Progress\OpenEdge\gui\fn  4  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   \  Q.  C:\Progress\OpenEdge\gui\set �  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    <  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i 4   ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    h   V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �   i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �   �j  C:\Progress\OpenEdge\gui\get $!  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    L!  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �!  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �!  Su  C:\Progress\OpenEdge\src\adm2\globals.i  "  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i <"  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   |"  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �"  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �"  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  <#  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  p#  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i �#  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �#  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   0$  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   h$  �$   d:\newsie\on_in_co\APLIC\sunat\check-impresoras.w        �        �$     �  $   �$            %  �   �     %     �      %  �   �     0%     �     @%  �   �     P%     O  #   `%  �   9     p%     7      �%  �   0     �%     .      �%  �   -     �%     +      �%  r        �%  n   �     �%     �  "   �%  i   �      &     x     &  P   _      &  �   V     0&     �  !   @&  �   �     P&     �     `&  �   �     p&     �     �&  �   �     �&     �     �&  g   v     �&     W     �&  O   ?     �&  �   �     �&     �      �&  �   �      '     ?     '  �   4      '          0'  �        @'     �     P'  �   �     `'     �     p'  �   �     �'     �     �'  �   �     �'     v     �'  �   s     �'     Q     �'  }   E     �'     #     �'     �      (     Y     (     
      (  7   �     0(  �   �     @(  O   �     P(     �     `(     Y     p(  �        �(  �        �(  O   �
     �(     �
     �(     �
     �(  �   v
     �(  x   n
  
   �(  M   Y
     �(     H
      )     �	     )  a   �	  
    )  �  �	     0)     �	     @)  �  r	     P)  O   d	     `)     S	     p)     	     �)  �   /     �)          �)     V     �)  x   P     �)     7     �)     �     �)     �     �)     �      *     �     *  Q     
    *     #     0*     �  
   @*     �     P*     �  
   `*  f   �     p*     3  	   �*  "   �     �*     �     �*     �     �*  Z   i     �*     q     �*     2     �*          �*           +     �     +  '   �        +     @      0+            @+           