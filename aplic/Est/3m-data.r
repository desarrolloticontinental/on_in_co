	��VgyW�4  ? �              ;                                �6 34F0010Dutf-8 MAIN O:\on_in_co\aplic\Est\3m-data.w,, PROCEDURE ue-ventas,, PROCEDURE ue-procesar,, PROCEDURE ue-inventarios,, PROCEDURE initializeObject,, PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER       �$              ��              $ �$  X�              ��              �0  
  +   �� �  7   �� `  8   �� �   C   � |  D   `� `  E   �� $  F   �    G   � �  H   Ԯ <  I   � �+  J           �� (  ? �� 8#  iSO8859-1                                                                           �#   ! �                                      �                  t�   
                 H     |   �   �  ,$         �  �   h$      t$          �                                             PROGRESS                         �           
    
                    �              �                                                                                                     
                                                                                                         �             �         �       �  L  p#     �#  �  ��      �#         �             �!          �!      �   ,  �
      �  
    
                  �  \                                                                                                       �
          
  �  �
      T  
    
                  @               �                                                                                          �
          
  �  �
         
    
                  �  �             p                                                                                          �
          
  0  �
      �  
    
                  �  `                                                                                                       �
          
  �  �
      X  
    
                  D    	           �                                                                                          �
          
  �  �
        
    
                  �  �  
           t                                                                                          �
          
  4        �  
    
                  �  d                                                                                                                  
  �  '      \  
    
                  H               �                                                                                          '          
  �  5                               �  �             x                                                                                          5            8	  B      �                        �  h	             $	                                                                                          B            �	  P      `	  
    
                  L	  
             �	                                                                                          P          
  �
  ^      
  
    
                  �	  �
             |
                                                                                          ^          
  <  l      �
  
    
                  �
  l             (                                                                                          l          
  �  z      d                        P               �                                                                                          z            �  �                              �  �             �                                                                                          �            @  �      �                        �  p             ,                                                                                          �                �      h                        T  <             �                                                                                          �            �           INTEGRAL                         PROGRESS                         �     !!  �      !!                         �)�V            *!  ��                              �  �                      �  �  �     CODMATDESMATCODMARUNDSTKUNDCMPFACEQUCODCTACODNEWMONVTAPREVTAPREBASAFTIGVVINMN1CODCIAVINMN2CODFAMVCTMN1FCHACTCODPR1CODPR2VCTMN2ARTPROFCHUSALFCHUCMPPMAXMN1PMAXMN2PULTMN1PULTMN2USUARIOFCHINGFCHCESFCHALZCLFMATUNDBASSUBFAMCODBRRCODANTTIPARTFCHPRMDFCHPRMHFCHREAPESMATDETALLECANEMPALMACENESDESMARAFTISCPORISCPORVTATPOMRGCTOLISCTOPRMMRGUTIPORMAXFCHMPREUNDANTPREANTPREACTDSCTOSTPOPROPORIGVCTOTOTTPOSUMCTOUNDORDENORDLISORDTMPTPOARTTPOCMBPPCHR__01CHR__02CHR__03DEC__01DEC__02DEC__03DATE__01DATE__02DATE__03MRGUTI-AMRGUTI-BMRGUTI-CPREOFIUNDAUNDBUNDCFLGINTFLGPRECLASEFCHPROMCATCONTATIPROTDSCTOPROMINFORFLGINFORPROMDIVIPROMFCHDPROMFCHHPROMDTODTOVOLRDTOVOLDUNDALTDSCALTMRGALTPREALTLICENCIAPROMMINDIVIPROMMINFCHDPROMMINFCHHPROMMINDTOCODDIGESAVTODIGESALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKMINSTKMAXSTKREPDESCRIPCION-LARGADESCRIPCION-TECNICASW-WEBWEB-SUBCATEGORIALIBRE_D03LIBRE_D04LIBRE_D05PESOBRUTOPAQUETELARGOALTOANCHOCTOLISMARCOCTOTOTMARCOCODSSFAMCLFESPECIALFLGCOMERCIALLIBRE_C06LIBRE_C07LIBRE_C08LIBRE_C09LIBRE_C10                                                                       	          
                                                                                                                                                                                                                                      !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2         3          4          5         6          7          8         9          :          ;          <         =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          P          Q          R          S          T          U          V          W          X          Y          Z          [         \         ]         ^         _         `         a 
        b 
        c 
        d 
        e 
        f 
        g         h         i         j         k         l 
        m 
        n 
        o 
        p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �     A!  �      A!                         a��U            A!  Q�                              �  <                      �  L  z      CODALMDESCRIPCIONCODCIATDOARTAUTMOVDIRALMHORRECENCALMTELALMCORRSALCORRINGCORRTRFCODDIVCLAVEALMCSGTPOCSGCODCLIFLGREPCAMPO-C                                                                        	          
                                                                                                     
             P!  �      P!                         a��U            Y!  )
                              �  ,                      h  <  )      CODCIACODALMUNDVTASTKACTCTOUNIFECHACODMAT                                                                                      estavtas                         PROGRESS                                 }"  �      }"                         =:�U            }"  .                              �  �                      X  �  �      CODDIVCODMATIMPNACCIGVIMPNACSIGVIMPEXTCIGVIMPEXTSIGVCANTIDADCOSTONACCIGVCOSTONACSIGVCOSTOEXTCIGVCOSTOEXTSIGVPROMNACCIGVPROMNACSIGVPROMEXTCIGVPROMEXTSIGVDATEKEYDIVDESTIPODELIVERYLISTABASE                                                                        	          
                                                                                                                                      ��                                               ��          �   0!  P ��            
                                                                                 
             
             
                                         
                                                                                                                P   `   p   �   �   �   �   �   �   �   �           0  @  P  `  p      P   `   p   �   �   �   �   �   �   �   �          0  @  P  `  p                                                                                              �"  �"  �"  �"  �"                        �"  #  #  #  #                         #  ,#  8#  D#                              H#  T#  `#  l#                                                                          tt-codmat   X(6)    Codigo Articulo Codigo Articulo     tt-coddiv   x(5)    C.Div   C.Div       tt-cantidad ->>,>>9.99  tt-cantidad 0   tt-importe  ->>,>>9.99  tt-importe  0   �  ���������           �"                �     i  i     	 	    �  �  �  �    ��                                               �          ����                            #   �q    "#   !�    (#    {    0#    �'    �"         undefined                                                               �       ��  �   l   �    4�                  �����               �p                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     7          assignFocusedWidget         �      �            LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    $       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    6       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          L       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    X       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    d       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    w       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    #      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    7      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    E      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    U      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    f      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    s      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    �
  $  �   �
  ���                       d                          � ߱             b  �
  �
  �  �       4   �����       o   c       (                              �  �   NA  �   �  �   �  �      �              0    D    X    l  `  �  
`  �  $  �    �     �      $  t  �  ���                       �     
                    � ߱        0�    �    �      �      4   �����                �                      ��                  �  �                  �0�                       �  ,  ,    �  �  �             4   ����       $  �     ���                       p  @         \              � ߱              �  H  X      �      4   �����      $  �  �  ���                         @         �              � ߱        assignPageProperty                              H  0      ��                  +  .  `              ��v                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             x               ��                  �           ��                            ����                            changePage                              �  �      ��                  0  1  �              X�v                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �  �      ��                  3  5  �              ��v                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            constructObject                             �  �      ��                  7  <  �              `�v                    O   ����    e�          O   ����    R�          O   ����    ��            ��   (             �               �� 
  P               
             ��   x             D               �� 
                 l  
         ��                            ����                            createObjects                               h  P      ��                  >  ?  �              ��a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              h  P      ��                  A  C  �              @�a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            destroyObject                               �  |      ��                  E  F  �              �a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �  |      ��                  H  J  �              |�a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  �      ��                  L  M  �              �a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               �  �      ��                  O  P  �              @�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               �  �      ��                  R  T  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            notifyPage                              �  �      ��                  V  X                H�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  ,           ��                            ����                            passThrough                             $        ��                  Z  ]  <              t�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             T               ��                  |           ��                            ����                            removePageNTarget                               |  d      ��                  _  b  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             �  
             ��                  �           ��                            ����                            selectPage                              �  �      ��                  d  f  �              �'�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            toolbar                             �  �      ��                  h  j                 �<�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            viewObject                              !   !      ��                  l  m  0!              �@�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                "   "      ��                  o  q  0"              XC�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  H"           ��                            ����                            disablePagesInFolder    
      �"      �"           LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �"      #      H#          LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  (#      t#      �#    )      HANDLE, getCallerWindow �#      �#      �#    <      HANDLE, getContainerMode    �#      �#      $    L      CHARACTER,  getContainerTarget  �#      ($      \$    ]      CHARACTER,  getContainerTargetEvents    <$      h$      �$    p      CHARACTER,  getCurrentPage  �$      �$      �$    �      INTEGER,    getDisabledAddModeTabs  �$      �$      $%     �      CHARACTER,  getDynamicSDOProcedure  %      0%      h%  !  �      CHARACTER,  getFilterSource H%      t%      �%  "  �      HANDLE, getMultiInstanceActivated   �%      �%      �%  #  �      LOGICAL,    getMultiInstanceSupported   �%      �%      0&  $  �      LOGICAL,    getNavigationSource &      <&      p&  %  
      CHARACTER,  getNavigationSourceEvents   P&      |&      �&  &        CHARACTER,  getNavigationTarget �&      �&      �&  '  8      HANDLE, getOutMessageTarget �&       '      4'  (  L      HANDLE, getPageNTarget  '      <'      l'  )  `      CHARACTER,  getPageSource   L'      x'      �'  *  o      HANDLE, getPrimarySdoTarget �'      �'      �'  +  }      HANDLE, getReEnableDataLinks    �'      �'      $(  ,  �      CHARACTER,  getRunDOOptions (      0(      `(  -  �      CHARACTER,  getRunMultiple  @(      l(      �(  .  �      LOGICAL,    getSavedContainerMode   |(      �(      �(  /  �      CHARACTER,  getSdoForeignFields �(      �(       )  0  �      CHARACTER,  getTopOnly   )      ,)      X)  1 
 �      LOGICAL,    getUpdateSource 8)      d)      �)  2  �      CHARACTER,  getUpdateTarget t)      �)      �)  3  
      CHARACTER,  getWaitForObject    �)      �)      *  4        HANDLE, getWindowTitleViewer    �)      *      P*  5  +      HANDLE, getStatusArea   0*      X*      �*  6  @      LOGICAL,    pageNTargets    h*      �*      �*  7  N      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �*      �*      ,+  8  [      LOGICAL,INPUT h HANDLE  setCallerProcedure  +      D+      x+  9  k      LOGICAL,INPUT h HANDLE  setCallerWindow X+      �+      �+  :  ~      LOGICAL,INPUT h HANDLE  setContainerMode    �+      �+      ,  ;  �      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  �+      4,      h,  <  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  H,      �,      �,  =  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �,      �,      -  >  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �,      @-      x-  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource X-      �-      �-  @  �      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �-      �-      .  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �-      <.      x.  B        LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   X.      �.      �.  C  ,      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �.      /      H/  D  F      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   (/      l/      �/  E  Z      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �/      �/       0  F  t      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �/       0      T0  G  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  40      t0      �0  H  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �0      �0      �0  I  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �0      1      L1  J  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    ,1      t1      �1  K  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �1      �1      2  L  �      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �1      (2      X2  M  �      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  82      |2      �2  N        LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �2      �2      3  O        LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �2      43      h3  P  '      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  H3      �3      �3  Q 
 ;      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �3      �3      4  R  F      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget �3      44      d4  S  V      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    D4      �4      �4  T  f      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �4      �4      5  U  w      LOGICAL,INPUT phViewer HANDLE   getObjectType   �4      45      d5  V  �      CHARACTER,  setStatusArea   D5      p5      �5  W  �      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             T6  <6      ��                  �  �  l6              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               X7  @7      ��                  �  �  p7              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                \8  D8      ��                  �  �  t8              8��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                d9  L9      ��                  �  �  |9              P��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               h:  P:      ��                  �  �  �:              0��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �:           ��                            ����                            getAllFieldHandles  �5       ;      4;  X  �      CHARACTER,  getAllFieldNames    ;      @;      t;  Y  �      CHARACTER,  getCol  T;      �;      �;  Z  �      DECIMAL,    getDefaultLayout    �;      �;      �;  [  �      CHARACTER,  getDisableOnInit    �;      �;      (<  \  �      LOGICAL,    getEnabledObjFlds   <      4<      h<  ]  �      CHARACTER,  getEnabledObjHdls   H<      t<      �<  ^        CHARACTER,  getHeight   �<      �<      �<  _ 	       DECIMAL,    getHideOnInit   �<      �<      =  `  #      LOGICAL,    getLayoutOptions    �<      (=      \=  a  1      CHARACTER,  getLayoutVariable   <=      h=      �=  b  B      CHARACTER,  getObjectEnabled    |=      �=      �=  c  T      LOGICAL,    getObjectLayout �=      �=      >  d  e      CHARACTER,  getRow  �=      $>      L>  e  u      DECIMAL,    getWidth    ,>      X>      �>  f  |      DECIMAL,    getResizeHorizontal d>      �>      �>  g  �      LOGICAL,    getResizeVertical   �>      �>      ?  h  �      LOGICAL,    setAllFieldHandles  �>      ?      D?  i  �      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    $?      d?      �?  j  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    x?      �?      �?  k  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �?      @      D@  l  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   $@      d@      �@  m  �      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    t@      �@      �@  n  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �@      A      <A  o  	      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal A      `A      �A  p   	      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   tA      �A      �A  q  4	      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �A      B      PB  r  F	      LOGICAL,    getObjectSecured    0B      \B      �B  s  Z	      LOGICAL,    createUiEvents  pB      �B      �B  t  k	      LOGICAL,    bindServer                              hC  PC      ��                  �  �  �C              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               lD  TD      ��                  �  �  �D              \�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             tE  \E      ��                  �  �  �E              p�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                |F  dF      ��                  �  �  �F              �&�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �G  pG      ��                  �  �  �G              �'�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �H  xH      ��                  �  �  �H              �*�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �I  |I      ��                  �  �  �I              4+�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            startServerObject                               �J  �J      ��                  �  �  �J              �+�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �K  �K      ��                  �  �  �K              P��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �K           ��                            ����                            getAppService   �B      `L      �L  u  z	      CHARACTER,  getASBound  pL      �L      �L  v 
 �	      LOGICAL,    getAsDivision   �L      �L      M  w  �	      CHARACTER,  getASHandle �L      M      <M  x  �	      HANDLE, getASHasStarted M      DM      tM  y  �	      LOGICAL,    getASInfo   TM      �M      �M  z 	 �	      CHARACTER,  getASInitializeOnRun    �M      �M      �M  {  �	      LOGICAL,    getASUsePrompt  �M      �M      ,N  |  �	      LOGICAL,    getServerFileName   N      8N      lN  }  �	      CHARACTER,  getServerOperatingMode  LN      xN      �N  ~  �	      CHARACTER,  runServerProcedure  �N      �N      �N    
      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �N      4O      dO  �  '
      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   DO      �O      �O  �  5
      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �O      �O      P  �  C
      LOGICAL,INPUT phASHandle HANDLE setASInfo   �O      ,P      XP  � 	 O
      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    8P      xP      �P  �  Y
      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �P      �P      Q  �  n
      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �P      $Q      XQ  �  }
      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  8Q      |Q      �Q  �  �
      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             pR  XR      ��                  �  �  �R              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �R             �R  
             ��   �R             �R               �� 
                 �R  
         ��                            ����                            addMessage                              �S  �S      ��                  �  �   T              �-�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   LT             T               ��   tT             @T               ��                  hT           ��                            ����                            adjustTabOrder                              dU  LU      ��                  �  �  |U              t5�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �U             �U  
             �� 
  �U             �U  
             ��                  �U           ��                            ����                            applyEntry                              �V  �V      ��                  �  �  �V              �<�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  W           ��                            ����                            changeCursor                                X  �W      ��                  �  �   X              H=�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8X           ��                            ����                            createControls                              4Y  Y      ��                  �  �  LY              X��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               8Z   Z      ��                  �  �  PZ              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                <[  $[      ��                  �  �  T[              D��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              H\  0\      ��                  �  �  `\              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              H]  0]      ��                  �  �  `]              (��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              H^  0^      ��                  �  �  `^              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                P_  8_      ��                  �  �  h_              0��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              X`  @`      ��                  �  �  p`              |��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �`             �`  
             ��   �`             �`               ��   a             �`               ��                   a           ��                            ����                            modifyUserLinks                             �a  �a      ��                  �  �  b              pP�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `b             ,b               ��   �b             Tb               �� 
                 |b  
         ��                            ����                            removeAllLinks                              xc  `c      ��                  �  �  �c              �X�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              xd  `d      ��                  �  �  �d              �\�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �d             �d  
             ��   e             �d               �� 
                 �d  
         ��                            ����                            repositionObject                                �e  �e      ��                       f              |c�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   \f             (f               ��                  Pf           ��                            ����                            returnFocus                             Hg  0g      ��                      `g              li�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 xg  
         ��                            ����                            showMessageProcedure                                |h  dh      ��                  	    �h              , �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �h             �h               ��                  �h           ��                            ����                            toggleData                              �i  �i      ��                      �i               �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �i           ��                            ����                            viewObject                              �j  �j      ��                      k              l
�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �Q      dk      �k  � 
 �      LOGICAL,    assignLinkProperty  pk      �k      �k  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �k      (l      Xl  �        CHARACTER,  getChildDataKey 8l      dl      �l  �         CHARACTER,  getContainerHandle  tl      �l      �l  �  0      HANDLE, getContainerHidden  �l      �l      m  �  C      LOGICAL,    getContainerSource  �l      m      Pm  �  V      HANDLE, getContainerSourceEvents    0m      Xm      �m  �  i      CHARACTER,  getContainerType    tm      �m      �m  �  �      CHARACTER,  getDataLinksEnabled �m      �m      n  �  �      LOGICAL,    getDataSource   �m       n      Pn  �  �      HANDLE, getDataSourceEvents 0n      Xn      �n  �  �      CHARACTER,  getDataSourceNames  ln      �n      �n  �  �      CHARACTER,  getDataTarget   �n      �n      o  �  �      CHARACTER,  getDataTargetEvents �n      o      Ho  �  �      CHARACTER,  getDBAware  (o      To      �o  � 
 �      LOGICAL,    getDesignDataObject `o      �o      �o  �  	      CHARACTER,  getDynamicObject    �o      �o       p  �        LOGICAL,    getInstanceProperties   �o      p      Dp  �  .      CHARACTER,  getLogicalObjectName    $p      Pp      �p  �  D      CHARACTER,  getLogicalVersion   hp      �p      �p  �  Y      CHARACTER,  getObjectHidden �p      �p      q  �  k      LOGICAL,    getObjectInitialized    �p      q      Hq  �  {      LOGICAL,    getObjectName   (q      Tq      �q  �  �      CHARACTER,  getObjectPage   dq      �q      �q  �  �      INTEGER,    getObjectParent �q      �q      �q  �  �      HANDLE, getObjectVersion    �q      r      8r  �  �      CHARACTER,  getObjectVersionNumber  r      Dr      |r  �  �      CHARACTER,  getParentDataKey    \r      �r      �r  �  �      CHARACTER,  getPassThroughLinks �r      �r      �r  �  �      CHARACTER,  getPhysicalObjectName   �r      s      @s  �  	      CHARACTER,  getPhysicalVersion   s      Ls      �s  �        CHARACTER,  getPropertyDialog   `s      �s      �s  �  2      CHARACTER,  getQueryObject  �s      �s      �s  �  D      LOGICAL,    getRunAttribute �s      t      8t  �  S      CHARACTER,  getSupportedLinks   t      Dt      xt  �  c      CHARACTER,  getTranslatableProperties   Xt      �t      �t  �  u      CHARACTER,  getUIBMode  �t      �t      �t  � 
 �      CHARACTER,  getUserProperty �t      u      4u  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    u      \u      �u  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles tu      �u      �u  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    �u      v      <v  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry v      xv      �v  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �v      w      @w  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType     w      dw      �w  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  tw      �w      �w  �  �      CHARACTER,  setChildDataKey �w      �w      (x  �        LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  x      Px      �x  �        LOGICAL,INPUT plHidden LOGICAL  setContainerSource  dx      �x      �x  �  1      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �x      �x      4y  �  D      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled y      Xy      �y  �  ]      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ly      �y      �y  �  q      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �y      z      8z  �        LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  z      `z      �z  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   tz      �z      �z  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �z      {      D{  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ${      h{      �{  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject t{      �{      �{  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    �{      |      D|  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   $|      `|      �|  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    x|      �|      �|  �        LOGICAL,INPUT c CHARACTER   setLogicalVersion   �|      }      D}  �  #      LOGICAL,INPUT cVersion CHARACTER    setObjectName   $}      h}      �}  �  5      LOGICAL,INPUT pcName CHARACTER  setObjectParent x}      �}      �}  �  C      LOGICAL,INPUT phParent HANDLE   setObjectVersion    �}      ~      <~  �  S      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ~      d~      �~  �  d      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks x~      �~      �~  �  u      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �~            L  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ,      l      �  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      �      �  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �      �      P�  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   0�      t�      ��  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      Ԁ       �  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��       �      P�  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage 0�      ��      ��  �  	      LOGICAL,INPUT pcMessage CHARACTER   Signature   ��      ��      �  � 	       CHARACTER,INPUT pcName CHARACTER    �    )  L�  Ȃ      8      4   ����8                ؂                      ��                  *  W                  $ȇ                       *  \�        +  �  p�      H      4   ����H                ��                      ��                  ,  V                  �ȇ                       ,  �  ��    C  ��  �      \      4   ����\                (�                      ��                  O  Q                  ,ɇ                       O  ��         P                                  �     
  
       
           � ߱        ��  $  S  T�  ���                           $  U  ؄  ���                       D                         � ߱        �    [   �  ��      T      4   ����T                ��                      ��                  \   	                  �ɇ                       \  0�  ��  o   _   	   ,                                 8�  $   `  �  ���                       �  @         �              � ߱        L�  �   a  �      `�  �   b  \      t�  �   d  �      ��  �   f  D      ��  �   h  �      ��  �   j  ,      Ć  �   k  �      ؆  �   l  �      �  �   o  X       �  �   q  �      �  �   r  H	      (�  �   t  �	      <�  �   u  @
      P�  �   v  |
      d�  �   w  �
      x�  �   x  l      ��  �   ~  �      ��  �   �        ��  �   �  X      ȇ  �   �  �      ܇  �   �  @      ��  �   �  �      �  �   �  8      �  �   �  �      ,�  �   �  (      @�  �   �  �      T�  �   �        h�  �   �  L      |�  �   �  �      ��  �   �  �      ��  �   �  p      ��  �   �  �      ̈  �   �  �      ��  �   �  $      �  �   �  `      �  �   �  �      �  �   �        0�  �   �  T      D�  �   �  �      X�  �   �  �      l�  �   �        ��  �   �  D      ��  �   �  �      ��  �   �  �          �   �  �                      Ԋ          @�  (�      ��                  G	  u	  X�              ,��                    O   ����    e�          O   ����    R�          O   ����    ��      h     
                �                     �                         � ߱         �  $ [	  p�  ���                           O   s	  ��  ��  4               l�          \�  d�    L�                                             ��                            ����                                45      ��      �     6     t�                      V p�  �                     Ѝ    �	  ,�  ��      @      4   ����@                ��                      ��                  �	  
                  ���                       �	  <�  ̌  �   �	  �      ��  �   �	        �  �   �	  �      �  �   �	        �  �   �	  �      0�  �   �	        D�  �   �	  x      X�  �   �	  �      l�  �   �	  p      ��  �   �	  �      ��  �   �	  `      ��  �   �	  �      ��  �   �	  X          �   �	  �      ��    '
  �  h�      D      4   ����D                x�                      ��                  (
  �
                  �!�                       (
  ��  ��  �   *
  �      ��  �   +
        ��  �   ,
  �      Ȏ  �   -
         ܎  �   .
  |       ��  �   /
  �       �  �   0
  l!      �  �   1
  �!      ,�  �   2
  T"      @�  �   3
  �"      T�  �   4
  D#      h�  �   5
  �#      |�  �   6
  ,$      ��  �   7
  �$      ��  �   8
  $%      ��  �   9
  �%      ̏  �   :
  &      ��  �   ;
  �&      �  �   <
  '      �  �   =
  �'      �  �   >
  (      0�  �   ?
  �(      D�  �   @
  )      X�  �   A
  �)      l�  �   B
  �)      ��  �   C
  x*      ��  �   D
  �*          �   E
  p+      ĕ    �
  Đ  @�      �+      4   �����+                P�                      ��                  �
  t                  D��                       �
  Ԑ  d�  �   �
  8,      x�  �   �
  �,      ��  �   �
  0-      ��  �   �
  �-      ��  �   �
  .      ȑ  �   �
  �.      ܑ  �   �
   /      �  �   �
  </      �  �   �
  �/      �  �   �
  �/      ,�  �   �
  (0      @�  �   �
  �0      T�  �   �
  1      h�  �   �
  �1      |�  �   �
   2      ��  �   �
  t2      ��  �   �
  �2      ��  �   �
  d3      ̒  �   �
  �3      ��  �   �
  4      ��  �   �
  �4      �  �   �
  5      �  �   �
  x5      0�  �   �
  �5      D�  �   �
  �5      X�  �   �
  l6      l�  �   �
  �6      ��  �   �
  �6      ��  �   �
   7      ��  �   �
  \7      ��  �   �
  �7      Г  �   �
  �7      �  �   �
  8      ��  �   �
  �8      �  �   �
  �8       �  �   �
  �8      4�  �   �
  89      H�  �   �
  t9      \�  �   �
  �9      p�  �   �
  �9      ��  �   �
  (:      ��  �   �
  �:      ��  �   �
  ;      ��  �   �
  �;      Ԕ  �   �
  �;      �  �   �
  t<      ��  �   �
  �<      �  �   �
  l=      $�  �   �
  �=      8�  �   �
  d>      L�  �   �
  �>      `�  �   �
  ?      t�  �   �
  �?      ��  �      �?      ��  �     @      ��  �     L@          �     �@      �  $  �  �  ���                       (A     
                    � ߱        ��    �  8�  H�      4A      4   ����4A      /   �  t�     ��                          3   ����DA            ��                      3   ����dA  �    �  Ж  L�  8�  �A      4   �����A  	              \�                      ��             	     �  H                  h                       �  ��  p�  �   �  �A      ȗ  $  �  ��  ���                       B     
  
       
           � ߱        ܗ  �   �  ,B      4�  $   �  �  ���                       TB  @         @B              � ߱        �  $  �  `�  ���                       �B                         � ߱        C     
                �C                     �D  @        
 �D              � ߱        ��  V   �  ��  ���                        �D                     (E                     dE                         � ߱        �  $  �  �  ���                       $F     
                �F                     �G  @        
 �G              � ߱        ��  V     ��  ���                        �G     
                xH                     �I  @        
 �I              � ߱            V   ,  <�  ���                        
               �                      ��             
     J  �                  �i                       J  ̚  �I     
                PJ                     �K  @        
 `K          L  @        
 �K          dL  @        
 $L          �L  @        
 �L              � ߱            V   _  H�  ���                        adm-clone-props ��  ,�              �     7     `                          \  _                     start-super-proc    <�  ��  �           �     8                                  �                     ��    �  $�  4�      PP      4   ����PP      /      `�     p�                          3   ����`P            ��                      3   �����P  ��  $    ̝  ���                       �P                         � ߱        ��    *  �  ��  0�  �P      4   �����P                �                      ��                  +  /                  a                       +  $�  �P                     �P                     �P                         � ߱            $  ,  ��  ���                             0  L�  ��      Q      4   ����Q  0Q                         � ߱            $  1  \�  ���                       ��    8  П  ��  8�  DQ      4   ����DQ      $  9  �  ���                       dQ                         � ߱            �   V  xQ      �Q     
                4R                     �S  @        
 DS              � ߱        ܠ  V   j  L�  ���                        �  �   �  �S      ��      �  �      �S      4   �����S      /      H�     X�                          3   �����S            x�                      3   ���� T  D�  $  $  ��  ���                       T                         � ߱        HT     
                �T                     V  @        
 �U              � ߱        p�  V   .  �  ���                        P�    �  ��  �       V      4   ���� V                �                      ��                  �  �                  0�                       �  ��      g   �  0�         ���                           ��          ȣ  ��      ��                  �      �              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  $�     4�  HV                      3   ����0V  d�     
   T�                      3   ����TV         
   ��                      3   ����\V    ��                              ��        �                  ����                                        D�              9      ��                      g                               X�  g   �  h�          �	��                           0�           �  �      ��                  �  �  �              8�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  \�     l�  �V                      3   ����dV            ��                      3   �����V    ��                              ��        �                  ����                                        |�              :      ��                      g                               `�  g   �  p�          �	�                           8�          �  �      ��                  �  �   �              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  d�     t�  �V                      3   �����V            ��                      3   �����V    ��                              ��        �                  ����                                        ��              ;      ��                      g                               ��    �  |�  ��      �V      4   �����V                �                      ��                  �  �                  x�                       �  ��  t�  /   �  4�     D�                          3   �����V            d�                      3   ����W  p�  /  �  ��     ��  PW                      3   ����0W  �     
   Ъ                      3   ����XW  �         �                      3   ����`W  @�        0�                      3   ����tW            `�                      3   �����W  ��    �  ��  ��      �W      4   �����W      /  �  ȫ     ث  DX                      3   ����$X  �     
   ��                      3   ����LX  8�        (�                      3   ����TX  h�        X�                      3   ����hX            ��                      3   �����X        �  ��  Ĭ      �X      4   �����X      /  �  �      �   Y                      3   �����X  0�     
    �                      3   ����Y  `�        P�                      3   ����Y  ��        ��                      3   ����$Y            ��                      3   ����@Y  ��    �  ܭ  X�      dY      4   ����dY                h�                      ��                  �  �                  h�                       �  �      g   �  ��         �$�        tY                  H�          �   �      ��                  �      0�              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  t�     ��  �Y                      3   �����Y  ��     
   ��                      3   �����Y         
   ԯ                      3   �����Y    ��                            ����                                        ��              <      �                      g                               �     �  �Y                                     �Y     
                DZ                     �[  @        
 T[              � ߱        ��  V   f  ��  ���                        �[     
                $\                     t]  @        
 4]              � ߱        Ա  V   �  D�  ���                        X�    �  �   �      �]      4   �����]      $   �  ,�  ���                       �]  @         �]              � ߱        ,�  g   �  p�         �г        �]  �г        ^                  L�          �  �      ��                  �  �  4�              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �  h�  x�      ^      4   ����^      O  �  ������  (^    ��                            ����                                        ��              =      ��                      g                               ص  g   �  D�         6|�         <^                  �          ܴ  Ĵ      ��                  �  �  ��              |�                    O   ����    e�          O   ����    R�          O   ����    ��      $�    �  H^  }          O  �  ������  \^    ��                            ����                                        X�              >      <�                      g                               @�  g   �  �         "�                           ��          ��  p�      ��                 �  �  ��              ��v                    O   ����    e�          O   ����    R�          O   ����    ��      �  $  �  �  ���                       p^                         � ߱        H�  r   �                       �^          |^           �  d�  �      �^      4   �����^                �                      ��                  �  �                  `6�                       �  t�      $   �  �  ���                       �^  @         �^              � ߱                      |�                                           ��                              ��        �                  ����                            ��          �  H�         ?     ��                      g   ��                          H�  g   �  X�         "�                           ��          �  ع      ��                 �  	  �              �6�                    O   ����    e�          O   ����    R�          O   ����    ��                                                                                                                         � ߱        �  $       �   �                               �  ��  8�  _      4   ����_                ��                      ��                                      8'                         �      /     ��                                 3   ����$_                H�                      ��                                      �'                         ̻      	     |�                                          3   ����<_    ��                              ��        �                  ����                                        l�              @      ��                      g                                     $  d�  �      H_      4   ����H_                T�                      ��                  $  P                  |,                       $  t�  X_  @                     �_  @         p_          �_  @         �_              � ߱        ��  $   %  �  ���                       |�  g   +  ��         n �      }                      `�          0�  �      ��                  ,  0  H�              <(                    O   ����    e�          O   ����    R�          O   ����    ��      ��  /  -  ��                                 3   �����_        .  ��  ȿ      �_      4   �����_      O  /  ������  `    ��                            ����                                        ��              A      �                      g                               P�  g   5  ��         !��         `                  ��          ,�  �      ��                  5  7  D�              8/                    O   ����    e�          O   ����    R�          O   ����    ��      (`  @                         � ߱            $  6  \�  ���                         ��                            ����                                        ��              B      ��                      g                               ��  /   :  |�                                 3   ����0`        A  ��  $�      L`      4   ����L`                ��                      ��                  A  N                  �/                       A  ��                ��          ��  ��      ��                 E  L                  0                       E  4�      O   E    ��          O   E    ��      �  /   I  �                                 3   ����d`        J  8�  H�      �`      4   �����`      k   K  d�              }       n        �   adm-create-objects  �  |�                      C      �                               b                     disable_UI  ��  ��                      D      <                              u  
                   enable_UI   ��  T�                      E                                     �  	                   exitObject  `�  ��                      F      �                               �  
                   initializeObject    ��  $�                      G      �                              �                     ue-inventarios  8�  ��                   H     h                          d  �!                     ue-procesar ��   �          �         I     �                          �  "                     ue-ventas   �  h�          �*         J     �*                          �*  �"  	                    � ��   �   �   ���  �        �  8   ����    $�  8   ����    <�  8   ����   L�  8   ����   \�    d�  8   ����   t�  8   ����         ��  8   ����   ��  8   ����       8   ����       8   ����       ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  ��  �      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  H�  T�      returnFocus ,INPUT hTarget HANDLE   8�  |�  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    l�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  ,�  <�      removeAllLinks  ,   �  P�  `�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE @�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  D�  P�      hideObject  ,   4�  d�  |�      editInstanceProperties  ,   T�  ��  ��      displayLinks    ,   ��  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  �   �      applyEntry  ,INPUT pcField CHARACTER    �  L�  \�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER <�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  �   �      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE �  t�  ��      unbindServer    ,INPUT pcMode CHARACTER d�  ��  ��      startServerObject   ,   ��  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  �  $�      restartServerObject ,    �  8�  P�      initializeServerObject  ,   (�  d�  x�      disconnectObject    ,   T�  ��  ��      destroyServerObject ,   |�  ��  ��      bindServer  ,   ��  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  �   �      enableObject    ,    �  4�  D�      disableObject   ,   $�  X�  d�      applyLayout ,   H�  x�  ��      viewPage    ,INPUT piPageNum INTEGER    h�  ��  ��      viewObject  ,   ��  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  �  �      selectPage  ,INPUT piPageNum INTEGER    ��  <�  P�      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ,�  ��  ��      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  |�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  �   �      initPages   ,INPUT pcPageList CHARACTER �  L�  h�      initializeVisualContainer   ,   <�  |�  ��      hidePage    ,INPUT piPageNum INTEGER    l�  ��  ��      destroyObject   ,   ��  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  �   �      createObjects   ,    �  4�  D�      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE $�  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  ��   �      changePage  ,   ��  �  (�      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 %     adecomm/as-utils.w  
"   
   �    }        �
"     
   � �     %       	           �     }        �G� �   �G%              � �     %       	  %        %        %       %        %       %               %               %               %              %              %              %               %              
�        
"   
 
�    
"   
 
"   
 \    �             �            
"   
   �        P         �     }        �%              
"   
 
"   
 \    �        �     �        �    
"   
   �        �         �     }        �%              � 
" 
   
 � %              � �  �         X      $              
�    �    �      
"   
 �                      
�            � !   \
" 
   
 \
�H T   %              �     }        �GG %              � 
"  
 
   P �L 
�H T   %              �     }        �GG %              
"  	 
   �        �    7%               
"  	 
 x�           �    1� 1  
 x� <   � %               o%   o           � A    x
"  	 
 x�           P    1� B   x� <   � %               o%   o           � P   x
"  	 
 x�           �    1� W  
 x� <   � %               o%   o           � b   x
"  	 
 x�           8    1� n   x� <   � %               o%   o           � |   x
"  	 
 x�           �    1� �   x� <   � %               o%   o           � �   x
"  	 
 x�                1� �   x� �   � %               o%   o           %               
"  	 
 � �          �    1� �   � � �     
"  	 
 x�           �    1� �   x� <   � %               o%   o           � �  e x
"  	 
 x�           L    1� M   x� <   � %               o%   o           � \  [ x
"  	 
 x�           �    1� �   x� �   � %               o%   o           %               
"  	 
 x�           <	    1� �   x� �   � %               o%   o           %               
"  	 
 x�           �	    1� �   x� �   � %               o%   o           %              
"  	 
 � �          4
    1� �   � � �     
"  	 
 x�           p
    1� �  
 x� �   � %               o%   o           %               
"  	 
 x�           �
    1�    x� <   � %               o%   o           � A    x
"  	 
 � �          `    1� 	   � � �     
"  	 
 x�           �    1�    x� <   � %               o%   o           � /  t x
"  	 
 � �              1� �  
 � � �     
"  	 
 x�           L    1� �   x� <   � %               o%   o           � �  � x
"  	 
 x�           �    1� M   x� <   � %               o%   o           � A    x
"  	 
 x�           4    1� d  
 x� o   � %               o%   o           %               
"  	 
 ��           �    1� s   �� �   � %               o%   o           %               
"  	 
 ��           ,    1� {   �� <   � %               o%   o           � A    �
"  	 
 ��           �    1� �   �� <   � %               o%   o           o%   o           
"  	 
 ��               1� �  
 �� <   � %               o%   o           � A    �
"  	 
 ��           �    1� �   �� �  	 � %               o%   o           � �  / �
"  	 
 � �              1� �   � � �  	   
"  	 
 ��           @    1�    �� �  	 � o%   o           o%   o           � A    �
"  	 
 � �          �    1�    � � �  	   
"  	 
 ��           �    1� &   �� �  	 � o%   o           o%   o           � A    �
"  	 
 � �          d    1� 6   � � �     
"  	 
 � �          �    1� D   � � �  	   
"  	 
 � �          �    1� Q   � � �  	   
"  	 
 � �              1� ^   � � �  	   
"  	 
 ��           T    1� l   �� �   � o%   o           o%   o           %              
"  	 
 � �          �    1� }   � � �  	   
"  	 
 � �              1� �  
 � � �     
"  	 
 � �          H    1� �   � � �  	   
"  	 
 � �          �    1� �   � � �  	   
"  	 
 � �          �    1� �   � � �  	   
"  	 
 � �          �    1� �   � � �  	   
"  	 
 � �          8    1� �  	 � � �  	   
"  	 
 � �          t    1� �   � � �  	   
"  	 
 � �          �    1�    � � �  	   
"  	 
 ��           �    1�    �� <   � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
 \(�  L ( l       �        �    �� $   � P   �        �    �@    
� @  , 
�       �    �� -     p�               �L
�    %              � 8      �    � $         � 4          
�    � N     
"   
 �� @  , 
�       �    �� W  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"  	 
 ��           �    1� Q  
 �� <   � %               o%   o           � A    �
"  	 
 ��               1� \  
 �� <   � %               o%   o           o%   o           
"  	 
 ��           �    1� g   �� �   � %               o%   o           o%   o           
"  	 
 ��                1� p   �� �   � %               o%   o           %               
"  	 
 ��           |    1�    �� �   � %               o%   o           %               
"  	 
 �           �    1� �   � <   � %               o%   o           � A    �
"  	 
 ��           l    1� �   �� �   � %               o%   o           %              
"  	 
 ��           �    1� �   �� �   � %               o%   o           o%   o           
"  	 
 ��           d    1� �   �� <   � %               o%   o           o%   o           
"  	 
 ��           �    1� �  	 �� <   � %               o%   o           � A    �
"  	 
 ��           T    1� �   �� <   � %               o%   o           o%   o           
"  	 
 ��           �    1� �   �� <   � %               o%   o           o%   o           
"  	 
 ��           L    1� �   �� �   � %               o%   o           %               
"  	 
 ��           �    1� �   �� �   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"  	 
 ��           �    1�    �� �  	 � %               o%   o           � A    �
"  	 
 ��               1�    �� �  	 � %               o%   o           � A    �
"  	 
 ��           �    1� #   �� �   � %               o%   o           %               
"  	 
 �           �    1� 1   � �  	 � %               o%   o           � A    �
"  	 
 ��           p     1� @   �� �  	 � %               o%   o           � A    
"  	 
 ��           �     1� N   �� �   � %               o%   o           %               
"  	 
 ��           `!    1� \   �� �  	 � %               o%   o           � A    �
"  	 
 ��           �!    1� k   �� �  	 � %               o%   o           � A    �
"  	 
 ��           H"    1� z   �� �  	 � %               o%   o           � A    �
"  	 
 ��           �"    1� �   �� �  	 � %               o%   o           o%   o           
"  	 
 ��           8#    1� �   �� �  	 � %               o%   o           � A    �
"  	 
 �           �#    1� �   � �  	 � %               o%   o           � A    �
"  	 
 ��            $    1� �  	 �� �   � %               o%   o           %               
"  	 
 ��           �$    1� �   �� �   � %               o%   o           %               
"  	 
 ��           %    1� �   �� �   � %               o%   o           o%   o           
"  	 
 ��           �%    1� �   �� �   � %               o%   o           o%   o           
"  	 
 ��           &    1� �   �� �   � %               o%   o           %               
"  	 
 ��           �&    1� �   �� �   � %               o%   o           %               
"  	 
 ��           '    1�    �� �   � %               o%   o           %               
"  	 
 �           �'    1�    � '   � %               o%   o           %       
       
"  	 
 �            (    1� /   � '   � %               o%   o           o%   o           
"  	 
 ��           |(    1� ;   �� '   � %               o%   o           %              
"  	 
 ��           �(    1� G   �� '   � %               o%   o           o%   o           
"  	 
 ��           t)    1� S   �� '   � %               o%   o           %              
"  	 
 ��           �)    1� `   �� '   � %               o%   o           o%   o           
"  	 
 ��           l*    1� m   �� '   � %               o%   o           %              
"  	 
 ��           �*    1� u   �� '   � %               o%   o           o%   o           
"  	 
 �           d+    1� }   � �  	 � %               o%   o           � A    �P �L 
�H T   %              �     }        �GG %              
"  	 
 ��           ,,    1� �   �� o   � %               o%   o           %               
"  	 
 ��           �,    1� �   �� o   � %               o%   o           o%   o           
"  	 
 ��           $-    1� �   �� <   � %               o%   o           � A    �
"  	 
 ��           �-    1� �   �� <   � %               o%   o           � �  - �
"  	 
 ��           .    1� �   �� <   � %               o%   o           � A    �
"  	 
 ��           �.    1�    �� <   � %               o%   o           � /   �
"  	 
 � �          �.    1� M   � � �     
"  	 
 ��           0/    1� ^   �� <   � %               o%   o           � A    �
"  	 
 � �          �/    1� j  
 � � �     
"  	 
 � �          �/    1� u   � � �     
"  	 
 ��           0    1� �   �� �  	 � %               o%   o           � A    �
"  	 
 ��           �0    1� �   �� <   � %               o%   o           � A    �
"  	 
 ��           1    1� �   �� �   � %               o%   o           o%   o           
"  	 
 ��           �1    1� �   �� <   � %               o%   o           � �  ! �
"  	 
 ��           �1    1� �   �� <   � %               o%   o           � A    �
"  	 
 �           h2    1� �   � <   � %               o%   o           � �   �
"  	 
 �           �2    1�   	 � o   � %               o%   o           o%   o           
"  	 
 ��           X3    1�    �� �   � %               o%   o           %               
"  	 
 � �          �3    1� #   � � �     
"  	 
 ��           4    1� 1   �� <   � %               o%   o           � E   �
"  	 
 ��           �4    1� T   �� �  	 � %               o%   o           � A    �
"  	 
 ��           �4    1� a   �� �  	 � %               o%   o           � A    �
"  	 
 � �          l5    1� q   � � �     
"  	 
 � �          �5    1� �   � � �  	   
"  	 
 �           �5    1� �   � �   � o%   o           o%   o           %               
"  	 
 � �          `6    1� �   � � �     
"  	 
 � �          �6    1� �   � � �  	   
"  	 
 � �          �6    1� �   � � �  	   
"  	 
 � �          7    1� �   � � �  	   
"  	 
 � �          P7    1� �   � � �  	   
"  	 
 � �          �7    1�    � � �  	   
"  	 
 � �          �7    1�    � � �     
"  	 
 ��           8    1� )   �� <   � %               o%   o           � @  4 �
"  	 
 � �          x8    1� u   � � �     
"  	 
 � �          �8    1� �   � � �     
"  	 
 � �          �8    1� �   � � �     
"  	 
 � �          ,9    1� �   � � �  	   
"  	 
 � �          h9    1� �   � � �  	   
"  	 
 � �          �9    1� �   � � �  	   
"  	 
 � �          �9    1� �   � � �     
"  	 
 ��           :    1� �   �� �  	 � %               o%   o           � A    �
"  	 
 ��           �:    1� �   �� �  	 � %               o%   o           � A    �
"  	 
 ��           ;    1� �   �� �  	 � %               o%   o           � A    �
"  	 
 ��           x;    1�    �� �  	 � %               o%   o           � A    �
"  	 
 ��           �;    1� (   �� �   � %               o%   o           %               
"  	 
 ��           h<    1� 6   �� �   � %               o%   o           o%   o           
"  	 
 ��           �<    1� H   �� �   � %               o%   o           %               
"  	 
 ��           `=    1� X   �� �   � %               o%   o           %               
"  	 
 ��           �=    1� d   �� �   � %               o%   o           o%   o           
"  	 
 ��           X>    1�    �� �   � %               o%   o           %               
"  	 
 � �          �>    1� �   � � �  	   
"  	 
 ��           ?    1� �   �� �   � %               o%   o           %              
"  	 
 � �          �?    1� �   � � �  	   
"  	 
 � �          �?    1� �   � � �  	   
"  	 
 � �          @    1� �  
 � � �  	   
"  	 
 ��           @@    1� �   �� �  	 � %               o%   o           � (   �
"  	 
 ��           �@    1� �   �� �  	 � %               o%   o           � A    �
"   
    "    � %     start-super-proc u� %     adm2/smart.p \P �L 
�H T   %              �     }        �GG %              
"  	 
   �       �A    6� $     
"  	 
   
�         B    8
"  
 
   �         B    ��     }        �G 4              
"  
 
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout \
�H T   %              �     }        �GG %              
"   
 \
"   
 � 
"   
 \
"   
   (�  L ( l       �        hC    �� $   � P   �        tC    �@    
� @  , 
�       �C    �� -   \p�               �L
�    %              � 8      �C    � $         � 4          
�    � N   \
"   
 �p� @  , 
�       �D    �� �   �p�               �L"    , �   � !   �� #   � �     }        �A      |    "      � !   �%              (<   \ (    |    �     }        �A� %   �A"    �    "    \"    �  < "    \"    �(    |    �     }        �A� %   �A"    �
�H T   %              �     }        �GG %              
"   
 \
"   
 � 
"   
 \
"   
   (�  L ( l       �        pF    �� $   � P   �        |F    �@    
� @  , 
�       �F    �� -   \p�               �L
�    %              � 8      �F    � $         � 4          
�    � N   \
"   
 �p� @  , 
�       �G    �� 1  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 \
"   
 � 
"   
 \
"   
   (�  L ( l       �        HH    �� $   � P   �        TH    �@    
� @  , 
�       `H    �� -   \p�               �L
�    %              � 8      lH    � $         � 4          
�    � N   \
"   
 �p� @  , 
�       |I    �� �   �p�               �L
"   
 , 
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
   (�  L ( l       �         J    �� $   � P   �        ,J    �@    
� @  , 
�       8J    �� -     p�               �L
�    %              � 8      DJ    � $         � 4          
�    � N     
"   
 �p� @  , 
�       TK    �� W  
 �p�               �L%     SmartWindow 
"   
   p� @  , 
�       �K    �� n     p�               �L%      WINDOW  
"   
  p� @  , 
�       L    �� &    p�               �L%               
"   
  p� @  , 
�       xL    ��     p�               �L(        � A      � A      � A      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 \    �        XM    �� $   �
"   
   � 8      �M    � $         � 4          
�    � N   \
"   
   �        �M    �
"   
   �       N    /
"   
   
"   
   �       HN    6� $     
"   
   
�        tN    8
"   
   �        �N    �
"   
   �       �N    �
"   
   p�    � N   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 \    �        xO    �A"    �A
"   
   
�        �O    �@ � 
"   
 �"      �       }        �
"   
 � %              %                "    � %     start-super-proc t� %     adm2/appserver.p ���    � �     
�    �     }        �%               %      Server  - �     }        �    "    �� A    � %                   "    �� A    � %      NONE    p�,  8         $     "    �        � �   \
�    
�H T   %              �     }        �GG %              
"   
 \
"   
 � 
"   
 \
"   
   (�  L ( l       �        R    �� $   � P   �        R    �@    
� @  , 
�       R    �� -   \p�               �L
�    %              � 8      (R    � $         � 4          
�    � N   \
"   
 �p� @  , 
�       8S    �� �   �p�               �L"    , p�,  8         $     "    �        � �   \
�     "    � %     start-super-proc s� %     adm2/visual.p \�   �      �      �   ;   
�H T   %              �     }        �GG %              
"   
 \
"   
 � 
"   
 \
"   
   (�  L ( l       �        �T    �� $   � P   �        �T    �@    
� @  , 
�       �T    �� -   \p�               �L
�    %              � 8      �T    � $         � 4          
�    � N   \
"   
 �p� @  , 
�       �U    �� \   �p�               �L"    , � 
"    
 � %     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP \%     processAction   
�    %     CTRL-PAGE-DOWN  "    � %     start-super-proc s� %     adm2/containr.p %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    � �   �
�    � �   � A    �    � �     
�    � �   � %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents %     buildDataRequest ent0 A    �    � �   � 
�    � �   �%     modifyListProperty  
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
"    
 � 
"   
 � %     contextHelp 
"    
   
�    
�    %               
�H T   %              �     }        �GG %              
"   
 \
"   
 � 
"   
 \
"   
 �(�  L ( l       �        Z    �� $   � P   �         Z    �@    
� @  , 
�       ,Z    �� -   \p�               �L
�    %              � 8      8Z    � $         � 4   \     
�    � N   � 
"   
 �p� @  , 
�       H[    �� q   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 \
"   
 � 
"   
 \
"   
 \(�  L ( l       �        �[    �� $   � P   �         \    �@    
� @  , 
�       \    �� -   \p�               �L
�    %              � 8      \    � $         � 4   \     
�    � N   \
"   
 �p� @  , 
�       (]    �� (   �p�               �L%              (        �     }        �G� �   �G� 
"   
 \
"   
   �        �]    �%              
"   
 � 
"   
 � �     }        �%               
"   
 � %      CLOSE   %               �       "      �      (         "      %                  "      �     \�            B"          "    ��     � %     ue-procesar � .     � 
"   
 � 
"   
 �
"   
 \�        d_    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � X  	   %               
"   
 � 
�    %     createObjects    �     }        �%     initializeObject s�  �     }        �(        �     }        �G� �   �G� 
"   
 \
"   
   �     }        �
�    
"   
 \"    �"    \"      "      "      
"   
 
"   
   %      CLOSE   %               %      SUPER   �            B     +  � �     �             B     +  � �     �     ^   "  
    �    }        �� _      "      "          "      %              "        "      %              "      %               � �      "      c                              � �      p�  �b  �b         %                   "    %              "      �c                              �c                             � !  	   � �     �  `c  lc        "      $d                              Hd        0d  4d  8d          � !  	   <d         "      � !     �  �c  �c        "      �d                              �d        �d  �d  �d          � !     �d         %              � !     �  hd  td        %               %              %               %              %              "     � "    � &    &    &    &        %              %              %               "     � &    &              "      � I!         "      � M!     "     � "    � "    � "    � &    &    &    &    &    &    &    L    0        %              %              %              %              *         "  	    "          "  	  �%                    "      %                   "           � b!     "      "      $h        h  h  h          0h                              h         "      � d!     � j!     p�  �g  �g              � p!     "           � r!     "      "      �h        �h  �h  �h           i                              �h         "      � d!     � j!     p�  �h  �h           <   � p!        "      %              %       (            � t!     "      "      �i        �i  �i  �i           j                              �i         "      � d!     � j!     p�  �i  �i         "  	         � v!     "      "      �j        �j  �j  �j          �j                              �j         "      � d!     � j!     p�  Lj  Xj              � p!     "           � x!     "      "      �k        hk  lk  pk          �k                              tk         "      � d!     � j!     p�  k  (k          � $    x     L $    8      $   � p!        "    � z!   � � !          "    � M!   � � !          "    � M!   \ �     h     D               "    �� �!  % �      +  � M!   �     +  � M!   �      +  � z!     � �!   \"      0m                              � �!     p�  m  m         %               "    � �m        �m  �m  �m          �m         "    � � �!   � p�  dm  pm             "    �%                  "    ��     � "    � Tn        <n  @n  Dn          Hn         "    � � �!   � p�  n  n         "      �n                              � �!     p�  tn  �n         %                   "    �%               "      (o                              � �      p�  �n  o         %              "      �o                              � �      p�  \o  ho         %               "    � �o                             � �!   � p�  �o  �o         "      "      "      "          "    �%              � �!     �    }        ��           "    �%              % 	    ue-ventas \    "    �%              %     ue-inventarios  � �!     � #"  Y   "      �    }        �� _      "      "          "    �%              %                  "      %                  "      %                  "      %              "    "      %               � �      "      �r                              � �      p�  Xr  dr         %                   "    �%              "      ,s                              8s                             � !  	   � �     �  �r  �r        "      �s                              �s        �s  �s  �s          � !  	   �s         "      � !     �  Xs  ds        "      4t                              `t        @t  Dt  Ht          � !     Lt         %              � !     �  �s  �s        %               %              %               %              %              "     � "    � &    &    &    &        %              %                  %               %                   "  	    %                  "  	    %              %                   "      "  	         "    \ "    \"    � "    � &    &    &    &        %              %              "     �"     �&    &    &    &        %              %               *    "       "       %               %                    "      "            "      "       %               %                       "      &        "      &         "      %                   "           � b!     "      "      �x        �x  �x  �x          �x                              �x         "      � d!     � j!     p�  Hx  Tx              � p!     "           � r!     "      "      |y        dy  hy  ly          �y                              py         "      � d!     � j!     p�  y  $y           <   � p!        "      %              %       (            � t!     "      "      |z        dz  hz  lz          �z                              pz         "      � d!     � j!     p�  z  $z              � p!     "           � v!     "      "      L{        4{  8{  <{          X{                              @{         "      � d!     � j!     p�  �z  �z                    � p!     � �"     "           � x!     "      "      0|        |  |   |          <|                              $|         "      � d!     � j!     p�  �{  �{         "           � �"     "      "      �|        �|  �|  �|          �|                              �|         "      � d!     � j!     p�  �|  �|              � p!     "          "      %                    � �"     "      "      �}        �}  �}  �}          �}                              �}         "      � d!     � j!     p�  �}  �}                  "      "      %                   � �"     "      "      �~        �~  �~  �~          �~                              �~         "      � d!     � j!     p�  l~  x~         %                   � �"     "      "      �        |  �  �          �                              �         "      � d!     � j!     p�  0  <         "           � �"     "      "      P�        8�  <�  @�          \�                              D�         "      � d!     � j!     p�  �  �         � �"          � �"     "      "      �        �  ��  ��          �                               �         "      � d!     � j!     p�  ��  ��          � $    x     L $    8      $   � p!        "    � z!   � � !          "    � M!   � � !          "    �� M!   \     � �"     "      "      t�        \�  `�  d�          ��                              h�         "      � d!     � j!     p�  �  �         � �"  
        � �"     "      "      0�        �  �   �          <�                              $�         "      � d!     � j!     p�  ̂  ؂         � �"          � �"     "      "      �        ԃ  ؃  ܃          ��                              ��         "      � d!     � j!     p�  ��  ��         � �"          � �"     "      "      ��        ��  ��  ��          ��                              ��         "      � d!     � j!     p�  D�  P�         � �"          � �"     "      "      d�        L�  P�  T�          p�                              X�         "      � d!     � j!     p�   �  �         � �"          � �"     "      "       �        �  �  �          ,�                              �         "      � d!     � j!     p�  ��  ȅ         � �"          � �"     "      "      ܆        Ć  Ȇ  ̆          �                              І         "      � d!     � j!     p�  x�  ��         � �"          � �"     "      "      ��        ��  ��  ��          ��                              ��         "      � d!     � j!     p�  4�  @�         � �"      �     h     D               "    �� �"    �      +  � M!   �     +  � M!   �      +  � z!     � �!   \"      ��                              � �!     p�  p�  |�         %               "    � �        ��   �  �          �         "    � � �!   � p�  Ј  ܈             "    �%                  "    ��     � "    � ��        ��  ��  ��          ��         "    � � �!   � p�  |�  ��         "      �                              � �!     p�  ��  �         %                   "    �%               "      ��                              � �      p�  h�  t�         %              "      �                              � �      p�  Ȋ  Ԋ         %               "    � T�                             � �!   � p�  (�  4�         "      "      "      "          "    �%              � �!     �    }        ��                       �           �   l       ��                 W  {  �               H!                    O   ����    e�          O   ����    R�          O   ����    ��        $  f  �   ���                       M     
                    � ߱              g  (  �      dM      4   ����dM                �                      ��                  h  z                  |�                       h  8  �  �  i  �M            k  �  `      N      4   ����N                p                      ��                  l  y                  �                       l  �  �  o   m      ,                                 �  �   n  (N      �  �   o  TN      $  $  p  �  ���                       �N     
                    � ߱        8  �   q  �N      L  �   r  �N      `  �   u  �N          $   x  �  ���                       O  @         �N              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               X�                    O   ����    e�          O   ����    R�          O   ����    ��      o                      �          �  $  �    ���                       dO     
                    � ߱                  �  �                      ��                   �  �                  �                     �  4      4   �����O      $  �  �  ���                       �O     
                    � ߱        �    �  4  D      �O      4   �����O      /  �  p                               3   �����O  �  �   �  P          O   �  ��  ��  <P                               , �                          
                               �      ��                            ����                                                        �   l       ��                  [  b  �               (�g                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  h  u  �               �g                    O   ����    e�          O   ����    R�          O   ����    ��           r  �   �       �`      4   �����`      n   s     �          �`        t    ,      �`      4   �����`      �   t  �`    ��                            ����                                                      �   l       ��                  {  �  �               �*g                    O   ����    e�          O   ����    R�          O   ����    ��      a  �           a  �          (a  �          4a  �          @a  �              � ߱        �  Z   �  �    �        a                  �               �              �              �              �              �              � ߱        �  h   �  @   �        La                  
   �  �� �             Xa    ��                              ��        �                  ����                                            �           �   l       ��                  �  �  �               �+g                    O   ����    e�          O   ����    R�          O   ����    ��      �     �  da  }          O   �  ��  ��  xa    ��                            ����                                            �           �   l       ��                  �  �  �               Tp[                    O   ����    e�          O   ����    R�          O   ����    ��      �   /   �  �                                 3   �����a  H  $   �    ���                       �a  @         �a              � ߱            $   �  t  ���                       �a  @         �a              � ߱          ��                              ��        �                  ����                                            �           �   l       ���               �  d  �                �[                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                        b       
       
           � ߱        d  $  �  8  ���                       b                         � ߱        x  �   �  b      �  $  �  �  ���                       8b                         � ߱        (  $  �  �  ���                       Db                         � ߱        �  $  �  T  ���                       Pb                         � ߱        �  $  �  �  ���                       �b                         � ߱        0  $  �    ���                       �b                         � ߱        �  $  �  \  ���                       �b                         � ߱        �  o   �           �b                            $   �  �  ���                       $c  @        	 c              � ߱        �    �  0  �  �  8c      4   ����8c                �                      ��                  �  �                  |'_                       �  @      $  �  �  ���                       �c                         � ߱                      �                      ��                  �                     |+_                       �        $  �  �  ���                       Td                         � ߱        @  $      ���                       �d                         � ߱        �  $    l  ���                        e                         � ߱        �  $    �  ���                       e                         � ߱        H  $      ���                       (e                         � ߱        �  $    t  ���                       <e                         � ߱        d  $    �  ���                       Pe                         � ߱              t      $
          �	  �	      ��                    3  
              0#r                �       �      �  �       ��                            7   ����          ��               �e    �            @	                  6           x	   ��         d	  �e    �            @	                                                        de   pe                   �	  �	           |e  �e           �e  �e                      �	   �	        O   ����  e�          O   ����  R�          O   ����  ��      �
  $    P
  ���                       �e       	       	           � ߱              �
      �          h  P      ��                       �              �#r                $       |
      $  t       ��                            7   ����          ��                     �            �                  6           �   ��         �        �            �                                                        �e                 <  0           �e           �e                                  O   ����  e�          O   ����  R�          O   ����  ��              �  0      �e      4   �����e                @                      ��                                      ��s                         �  $  B          �   ��         �  �f                                         Df   Pf   \f   hf                                tf  �f  �f               |f  �f  �f  �f                      �   �            @  �      g      4   ����g                �                      ��                                      �5�                         P      $    �  ���                       g       	       	           � ߱              "  @  �      <g      4   ����<g                �                      ��                  "  2                  $6�                       "  P  $  $  #  �  ���                       dg                         � ߱        |  $  $  P  ���                       �g                         � ߱        �  $  &  �  ���                       �g                         � ߱        ,  $   '     ���                       Ph  @        	 <h              � ߱        �  $  (  X  ���                       ph                         � ߱        �  $   )  �  ���                        i  @        	 i              � ߱        4  $  *    ���                       pi                         � ߱        �  $   +  `  ���                        j  @        	 j              � ߱        �  $  ,  �  ���                       ,j                         � ߱        <  $   -    ���                       �j  @        	 �j              � ߱        �  $  .  h  ���                       �j                         � ߱            $   /  �  ���                       �k  @        	 �k              � ߱        D  $  7    ���                       dl                         � ߱        �  $   ;  p  ���                       Pm  @        	 <m              � ߱        �  �   <  �m      �    @  �  H  �  �m      4   �����m                X                      ��             	     @  F                  t7�                       @  �        B  t  �      �m      4   �����m  	                                     ��             	     B  E                  ,|�                       B  �    �   C  `n          $   D  @  ���                       �n  @        	 �n              � ߱        
                                      ��             
     G  L                  �|�                       G  l  �    M    �  T  �n      4   �����n                �                      ��                  M  O                  <}�                       M        $   N  �  ���                       Ho  @        	 4o              � ߱                      d                      ��                  P  S                  �}�                       P  �  �  $   Q  �  ���                       �o  @        	 �o              � ߱            �   R  �o      �  �  W  p  �  �  X  p     �  Y   p    �  Z  ,p  �    \  ,  �      8p      4   ����8p                �                      ��                  \  ^                  l~�                       \  <      	  ]  �                                        3   ����`p      �   b  lp                    D                                  �        �    ����             ��                             ��                             ��                            ����                                                  �           �   l       ��                 j  �  �               P�                    O   ����    e�          O   ����    R�          O   ����    ��      �    �  �   L      �p      4   �����p                \                      ��                  �  �                  4og                       �  �       /   �  �                                 3   �����p  |    �  �  0      �p      4   �����p                @                      ��                  �  �                  �og                       �  �      /   �  l                                 3   �����p      	   �  �                                          3   ����q                �                                           ��                            ����                                            �           �   l       ���+               �    �               `pg                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       q                         � ߱        d  $  �  8  ���                       (q                         � ߱        x  �   �  4q      �  $  �  �  ���                       Tq                         � ߱        (  $  �  �  ���                       `q                         � ߱        T    �  D  �  �  lq      4   ����lq                �                      ��                  �  �                  ��g                       �  T  (  $  �  �  ���                       �q                         � ߱            $  �  T  ���                       �q                         � ߱                      �                      ��                  �  �                  ��g                       �  �      $  �  (  ���                       �q                         � ߱        �  $  �  �  ���                       �q                         � ߱          $  �  �  ���                       ,r                         � ߱        \  $  �  0  ���                       8r                         � ߱        �  o   �           Lr                          �  $   �  �  ���                       �r  @        	 �r              � ߱        �    �    �  T  �r      4   �����r                �                      ��                  �  �                  l�q                       �        $  �  �  ���                       Ds                         � ߱                      d                      ��                  �  �                  D�q                       �  �      $  �  �  ���                       �s                         � ߱          $  �  �  ���                       lt                         � ߱        l  $  �  @  ���                       �t                         � ߱        �  $  �  �  ���                       �t                         � ߱        	  $  �  �  ���                       �t                         � ߱        t	  $  �  H	  ���                       �t                         � ߱        8
  $  �  �	  ���                       �t                         � ߱              H
      �          �  �      ��                  �  M  �              4�q                `$     �  �	      t
  �
       ��                            7   ����          ��               u    �                              6   �        L   ��         8  u    �                                                                    �t   �t                   �  �           �t  u           u  u                      h   |        O   ����  e�          O   ����  R�          O   ����  ��      t  �   �       �      �  �          \  D      ��       0    	         t              x�g     	 �u     �             $    �  ���                       Lu       	       	           � ߱        4  $      ���                       |u       	       	           � ߱            4   �����u      O   ����  e�          O   ����  R�          O   ����  ��      �  $    �  ���                       �u                         � ߱                   |       v      4   ���� v                �                      ��             	                         ܘg                                       �  �+      �  p      ��             	     
    �              @�g                       
  �      4  �       ��                            7   ����           ��               `v    �            �                  6   
            ��         �  `v    �            �                                                        (v   4v                   \  P           @v  Pv           Hv  Xv         �            (   <        O   ����  e�          O   ����  R�          O   ����  ��      �  A            ��           �v                                         �v   �v                   l  `           �v  �v           �v  �v         �            8   L    ,      �        �v      4   �����v  	              (                      ��             	                         \�q                         �  �  9        w                     w                     w                     0w                         � ߱            $    8  ���                       Dw                     dw                         � ߱            $    �  ���                       
      �      |          L  4      ��                    L  d              @�q                         X         P       ��                            7   ����        ����               �w    �            �                  6          �  ����         �  �w    �            �                                                        �w   �w                                                         @            �           O   ����
 
 e�          O   ����
 
 R�          O   ����
 
 ��      �  $    �  ���                       �w                         � ߱        ,  $       ���                       x                         � ߱        �  $    X  ���                       (x                         � ߱        �  $      �  ���                       �x  @        	 �x              � ߱        4  $  !    ���                       �x                         � ߱        �  $   "  `  ���                       �y  @        	 �y              � ߱        �  $  #  �  ���                       �y                         � ߱        <  $   $    ���                       �z  @        	 �z              � ߱        �  $  %  h  ���                       �z                         � ߱        �  $   &  �  ���                       x{  @        	 d{              � ߱        D  $  '    ���                       �{                         � ߱        �  $   (  p  ���                       \|  @        	 H|              � ߱        �  $  )  �  ���                       h|                         � ߱        L  $   *     ���                       }  @        	 }              � ߱        �    +  h  �    8}      4   ����8}                �                      ��                  +  .                  L�q                       +  x  L  $  ,     ���                       `}                         � ߱            $   -  x  ���                       ~  @        	 �}              � ߱                                             ��                  /  2                  (�q                       /  �  x  $  0  L  ���                       L~                         � ߱            $   1  �  ���                       �~  @        	 �~              � ߱        (  $  3  �  ���                                                � ߱        �  $   4  T  ���                       �  @        	 �              � ߱        �  $  5  �  ���                       �                         � ߱        0  $   6    ���                       |�  @        	 h�              � ߱        �  $  7  \  ���                       ��                         � ߱        �  $   8  �  ���                       8�  @        	 $�              � ߱        8  $  <    ���                       ��                         � ߱        �  $   =  d  ���                       ��  @        	 ��              � ߱        �  $  >  �  ���                       ��                         � ߱        @   $   ?     ���                       \�  @        	 H�              � ߱        �   $  @  l   ���                       h�                         � ߱        �   $   A  �   ���                       �  @        	 �              � ߱        H!  $  B  !  ���                       $�                         � ߱        �!  $   C  t!  ���                       Ԅ  @        	 ��              � ߱        �!  $  D  �!  ���                       ��                         � ߱        P"  $   E  $"  ���                       ��  @        	 |�              � ߱        �"  $  F  |"  ���                       ��                         � ߱         #  $   G  �"  ���                       L�  @        	 8�              � ߱        X#  $  H  ,#  ���                       X�                         � ߱        �#  $   I  �#  ���                       �  @        	 �              � ߱        $  $  J  �#  ���                       �                         � ߱            $   K  4$  ���                       ć  @        	 ��              � ߱        �$  $  Q  �$  ���                       Ї                         � ߱        %  $   U  �$  ���                       ��  @        	 ��              � ߱        $%  �   V   �      \'    Z  @%  �%  L'  4�      4   ����4�                �%                      ��                  Z  `                  �w                       Z  P%        \  �%  d&      \�      4   ����\�                t&                      ��                  \  _                  x�w                       \  �%  �&  �   ]  ̉          $   ^  �&  ���                       ,�  @        	 �              � ߱                                              ��                  a  f                  ܂w                       a  �&  D)    g  x'  �'  �(  @�      4   ����@�                (                      ��                  g  i                  `�w                       g  �'      $   h  0(  ���                       ��  @        	 ��              � ߱                      �(                      ��                  j  m                  ܃w                       j  \(  0)  $   k  )  ���                       �  @        	  �              � ߱            �   l  `�      T)  �  q  t�  d)  �  r  ��  t)  �  s  ��  �)  �  t  ��  p*    v  �)  *      ��      4   ������                ,*                      ��                  v  x                  ��w                       v  �)      	  w  `*                                        3   ����̋      �   |  ؋                    �*                                  �        ��     ����           ��                             ��                             ��                             ��                             ��                            ����                                =                 r   d d     $   ���  � �                                               �                                                                         d     D                                                                 P   �� �d                                                           �"  G   
 X  ��  d                                                         {     �  
    P   H� �d                                                           �"  G     �  H� @l                                             
           �     �  
           "                           "  '  )  /  1  7  9  @  B  L  N  V  Y  c  f  p       h  ��iM                                                        �     �     �"               h  ��M                                                        �     �     #               \  h�M                                 �                 #                @     
 X  ,k d                                                        s     �      \  �e�p                                 �                 #                @      P �I& >         p                                              �        D                                                                    TXS appSrvUtils ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST lCodProv s-codcia 10011922 tt-art-div tt-codmat tt-coddiv tt-cantidad tt-importe wWin BtnPath BtnProcesar cboMes Enero 1 Febrero 2 Marzo 3 Abril 4 Mayo 5 Junio 6 Julio 7 Agosto 8 Setiembre 9 Octubre 10 Noviembre 11 Diciembre 12 txtPath txtYear ChkInventarios ChkVentas fMain ->,>>>,>>9 yes/no X(256) Elija el directorio donde grabar el resultado GUI Informacion ( 3M ) DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   txtYear cboMes ChkVentas ChkInventarios BtnPath BtnProcesar CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE lDirectorio  Directorio Files Ingrese el directorio destinto iStartPage ADM-ERROR ADM-CREATE-OBJECTS DISABLE_UI ENABLE_UI EXITOBJECT >9 >999 INITIALIZEOBJECT lFileXls lNuevoFile lDia lMes lYear lFecha lFileNameXls lStock lFileName fullname \\192.168.100.251\newsie\on_in_co\Plantillas\001B1_INVENTARIO_PE20100038146_LIMA_PLANTILLA.xls GENERAL chExcelApplication chWorkbook chWorksheet chWorksheetRange iCount iIndex iColumn cColumn cRange lCerrarAlTerminar lMensajeAlTerminar Excel.Application Visible Workbooks OPEN Sheets Item Almmmatg Cat�logo de Materiales Almacen 11T 99 AlmStkal AlmStkal A Range Value ' B C D E 9999 - \001B1_INVENTARIO_PE20100038146_LIMA_ _000000 DisplayAlerts SaveAs Quit Proceso Terminado UE-INVENTARIOS lfilename Concluido... UE-PROCESAR lDiaKey lSec lDataKey \\192.168.100.251\newsie\on_in_co\Plantillas\001A1_VENTA_PE20100038146_LIMA_PLANTILLA.xls VentasxProducto Tda F G H I PEN J L 000-000000 M FA N NONE O P Q R S \001A1_VENTA_PE20100038146_LIMA_ UE-VENTAS idx01 A�o Mes Ventas Inventarios ... Procesar Matg13 alm01 llave01 Index01 8  p)  h  �0      ' �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   [	  s	  u	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props f  g  h  i  k  l  m  n  o  p  q  r  u  x  y  z  {              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  T	  �	     =                                   �  �  �	  �	     >                                   �  �  �            
     lDirectorio �	  L
     ?   �	                              �  �  �  �  �  �  
  �
     @                                                  	  d
  �
     A                                   -  .  /  0  �
  $     B                                   6  7  �
  p     C               \                  adm-create-objects  b  ,  �     D               �                  disable_UI  r  s  t  u  t  �     E               �                  enable_UI   �  �  �  �  �  H     F               <                  exitObject  �  �  �    �     G               �                  initializeObject    �  �  �  �  �        �     lFileXls    �        �     lNuevoFile          �     lDia                  lMes    <        4     lYear   X        P     lFecha  |        l     lFileNameXls    �     	   �     lStock  �     
   �     lFileName   �        �     fullname             �     chExcelApplication                chWorkbook  @        4     chWorksheet h        T     chWorksheetRange    �        |     iCount  �        �     iIndex  �        �     iColumn �        �     cColumn �        �     cRange               lCerrarAlTerminar             0     lMensajeAlTerminar  T  �  K   H   �          t                  ue-inventarios  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                                      "  #  $  &  '  (  )  *  +  ,  -  .  /  2  3  7  ;  <  @  B  C  D  E  F  G  L  M  N  O  P  Q  R  S  W  X  Y  Z  \  ]  ^  b  d  �        �     fullname              �     lfilename   D  ,     I   �                             ue-procesar �  �  �  �  �  �  �  �  l        `     lFileXls    �        �     lNuevoFile  �        �     lDia    �        �     lMes    �        �     lYear   �        �     lFecha               lDiaKey 4     	   ,     lSec    T     
   H     lDataKey    x        h     lFileNameXls    �        �     lFileName   �        �     fullname    �        �     chExcelApplication           �     chWorkbook                chWorksheet H        4     chWorksheetRange    d        \     iCount  �        x     iIndex  �        �     iColumn �        �     cColumn �        �     cRange  �        �     lCerrarAlTerminar                  lMensajeAlTerminar  �  `  v   J   L          T                  ue-ventas   �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �        
                               !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /  0  1  2  3  4  5  6  7  8  <  =  >  ?  @  A  B  C  D  E  F  G  H  I  J  K  L  M  Q  U  V  Z  \  ]  ^  _  `  a  f  g  h  i  j  k  l  m  q  r  s  t  v  w  x  |    $  �       �      (                          x  �     tt-art-div  �         �         �         �         tt-codmat   tt-coddiv   tt-cantidad tt-importe            �  
   appSrvUtils $            lCodProv    D        8     s-codcia    `       X  
   wWin    |       t     cboMes  �       �     txtPath �       �     txtYear �       �     ChkInventarios  �       �     ChkVentas              
   gshAstraAppserver   H        4  
   gshSessionManager   l        \  
   gshRIManager    �        �  
   gshSecurityManager  �  	 	     �  
   gshProfileManager   �  
 
     �  
   gshRepositoryManager            �  
   gshTranslationManager   8        (  
   gshWebManager   \        L     gscSessionId    �        p     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager                 gsdTempUniqueID 0        $     gsdUserObj  X        D     gsdRenderTypeObj    �        l     gsdSessionScopeObj  �       �  
   ghProp  �    	   �  
   ghADMProps  �    
   �  
   ghADMPropsBuf          �     glADMLoadFromRepos  $            glADMOk D       8  
   ghContainer d       X     cObjectName �       x     iStart  �       �     cAppService �       �     cASDivision �       �     cServerOperatingMode                 cFields               iStartPage  D    L  8  tt-art-div  `       T  Almmmatg    x       p  Almacen �       �  AlmStkal              �  VentasxProducto          7   �   b  c  t  �  �  �  �  �  �  �  )  *  +  ,  C  O  P  Q  S  U  V  W  [  \  _  `  a  b  d  f  h  j  k  l  o  q  r  t  u  v  w  x  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �   	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  '
  (
  *
  +
  ,
  -
  .
  /
  0
  1
  2
  3
  4
  5
  6
  7
  8
  9
  :
  ;
  <
  =
  >
  ?
  @
  A
  B
  C
  D
  E
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
           t  �  �  �  �  �  �  �  �  �  �  �  �    ,  H  J  _  �  �       *  +  ,  /  0  1  8  9  V  j  �       $  .  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  f  �  �  �  �  �  �  �  $  %  +  5  :  A  E  I  J  K  L  N  P      �� & O:\on_in_co\APLIC\lib\excel-close-file.i �   r� % O:\on_in_co\APLIC\lib\excel-open-file.i   !  H� $ C:\Progress\OpenEdge\src\adm2\windowmn.i 4!  f!  C:\Progress\OpenEdge\src\adm2\containr.i h!  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �!  ��  C:\Progress\OpenEdge\src\adm2\visual.i   �!  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  "  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    T"  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �"  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �"  Ds   C:\Progress\OpenEdge\gui\fn  #  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   ,#  Q.  C:\Progress\OpenEdge\gui\set l#  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �#  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �#  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    $  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  P$  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i �$  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �$  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i %  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    8%  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    |%  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �%  �j  C:\Progress\OpenEdge\gui\get �%  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    &  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    `&  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �&  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �&  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i '  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   L'  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �'  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �'  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  (  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  @(  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i �(  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �(  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i    )  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   8)  ��    O:\on_in_co\aplic\Est\3m-data.w      �  {      �)     Y  &   �)  a  �      �)     �  %   �)  �  a      �)     ?  &   �)  �  
      �)     �  %   *  .  S      *       $   ,*  �   �      <*  �   �     L*     �     \*  �   �     l*     ^     |*  �   V     �*     �  #   �*  �   �     �*     �      �*  �   �     �*     �      �*  �   �     �*     �      �*  r   �     +  n   �     +     M  "   ,+  i   H     <+     &     L+  P        \+  �        l+     �  !   |+  �   �     �+     �     �+  �   �     �+     b     �+  �   `     �+     >     �+  g   $     �+          �+  O   �     ,  �   w     ,     u      ,,  �   E     <,     �     L,  �   �     \,     �     l,  �   �     |,     �     �,  �   �     �,     z     �,  �   y     �,     W     �,  �   F     �,     $     �,  �   !     �,     �     -  }   �     -     �     ,-     U     <-          L-     �     \-  7   }     l-  �   t     |-  O   f     �-     U     �-          �-  �   �
     �-  �   �
     �-  O   �
     �-     �
     �-     I
     �-  �   $
     .  x   
  
   .  M   
     ,.     �	     <.     �	     L.  a   �	  
   \.  �  r	     l.     S	     |.  �   	     �.  O   	     �.     	     �.     �     �.  �   �     �.     �     �.          �.  x   �     �.     �     /     n     /     j     ,/     V     </     =     L/  Q   -  
   \/     �     l/     �  
   |/     �     �/     m  
   �/  f   B     �/     �  	   �/  "   �     �/     �     �/     h     �/  Z        �/          0     �     0     �     ,0     �     <0     |     L0  '   �       \0     @      l0            |0           