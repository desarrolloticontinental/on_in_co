	��V�:�ax6  ��              �                                �z 36780114utf-8 MAIN d:\newsie\on_in_co\APLIC\vta2\w-resumen-cotizaciones.w,, PROCEDURE um-saldos-almacen,,INPUT p-almacen CHARACTER,INPUT p-codmat CHARACTER,INPUT p-fecha DATE,OUTPUT p-Stock DECIMAL PROCEDURE um-procesar,, PROCEDURE um-excel,, PROCEDURE um-calc-saldos-art,, PROCEDURE recoge-parametros,, PROCEDURE procesa-parametros,, PROCEDURE initializeObject,, PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE _corre-program,, PROCEDURE _busca-lookup,,INPUT campo_name CHARACTER,INPUT program_call CHARACTER,OUTPUT program_name CHARACTER PROCEDURE adm-imprime,, PROCEDURE adm-busca,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER      \J              �:             �G \J  8�               �              $3    +   �� `     � `     t� �  	   `� l  
   �� �  A   l� `  B   �� �   M   �� |  N   <� `  O   �� $  P   �� (  Q   ��    R   �    S   (� �  T   �� �  U   �
 �  V   P   W           h ,  ? �" %  iSO8859-1                                                                           LI   + �           �                         �     
             l�                     B     4B   �A   $�  �I         ��  �   $J      0J          t                                             PROGRESS                         �           
    
                    �              �                                                                                                     
                                                                                                         �             �             �                                                                                          �             �         �       �  L  �H     �H  �  =�      (I  	       �             \D          $E      �     �            $  �            8  �            L  �            `  �                �  	                       INTEGRAL                         PROGRESS                         �     �  t      �                         �ɺ[            �  b|                              �  D                      �  T  ,      PROGRAMA_LOOKUPPROGRAMA_CALLCAMPODESCRIPCION                                        \  �      �  
    
                  �  �  	           H                                                                                          �          
          �  
    
                  p  8  
           �                                                                                                    
  �        0  
    
                    �             �                                                                                                    
  `         �  
    
                  �  �             L                                                                                                     
    3      �  
    
                  t  <             �                                                                                          3          
  �  E      4  
    
                     �             �                                                                                          E          
  d	  Z      �  
    
                  �  �	             P	                                                                                          Z          
  
  p      �	  
    
                  x	  @
             �	                                                                                          p          
  �
  ~      8
                         $
  �
             �
                                                                                          ~            h  �      �
                        �
  �             T                                                                                          �              �      �  
    
                  |  D                                                                                                        �          
  �  �      <  
    
                  (  �             �                                                                                          �          
  l  �      �  
    
                  �  �             X                                                                                          �          
    �      �                        �  H                                                                                                       �            �  �      @                        ,  �             �                                                                                          �            p  �      �                        �  �             \                                                                                          �                �      �                        �                                                                                                         �                 e   t      e                          �M�]            m   ��                              �  �                      �  �  E-     REFERENCIASCODPRONOMPRODIRPRORUCCODCIATPOPROLOCPROFAXPROFCHINGFLGSITCODPOSCODPAISCODDEPTCODPROVCODDISTGIRPROCNDCMPCONTACTOSTELFNOSE-MAILFCHACTCLFPROUSUARIORUCOLDAPEPATAPEMATNOMBREPERSONATPOENTPRIORIDADPAGOREPLEGALREPLEGALCARGOLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKMINSTKMAXSTKREP                                                                         	          
                                                                                                                                                                                                                                     !          "          #          $          %          &          '          (          )          *          +          ,          -          .          �     �   t      �                          �M�]            �   ~                              �  �                      �  �  �3     CODDIVDESDIVCODCIADIRDIVTELDIVFAXDIVRESPONFLGREPFLGPREUNIFLGAPRCOTFLGAPRPEDFLGPREVTADIASVTOCOTDIASVTOPEDDIASVTOO_DDIASAMPCOTTIPDTOMODPREUNIFLGEMPAQUEFLGROTACIONCANALVENTAFLGPICKINGFLGBARRASFLGMINVENTAFLGDTOPROMFLGDTOVOLFLGTARJETAVENTAMOSTRADORVENTAMAYORISTAVENTAMINORISTAFLGDTOCLFCLIPORDTOCLFCLIFLGDTOCNDVTALIBRE_C01LIBRE_C02LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02FLAGTIPOFLAGUSUARIOFLAGMIGRACIONFLAGFECHACAMPO-LOGCAMPO-DECCAMPO-DATECAMPO-CHARGRUPO_DIVI_GGCENTRO_COSTO                                                                       	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          .          /          0 
        1 
        2 
        3 
        4          5          �&  $   �#  t      �#                         ��{a            �#  ��                              �  h                      �  x  #�     CODCIACODDOCNROPEDFCHPEDCODCLINOMCLIDIRCLIRUCCLIHORAFLGESTCODALMCODMONTPOCMBUSUARIOGLOSAIMPBRTIMPEXOPORIGVIMPDTOIMPTOTIMPIGVCODVENIMPISCIMPVTAFLGSITFMAPGOFCHVENORDCMPCODDIVTIPVTAPORDTOTPOPEDUSRDSCTOLUGENTCODTRANSNROREFCMPBNTEOBSERVANCMPBNTEATENCIONFAXCLILUGENT2IMPFLEFLGIGVTPOLICUBIGEOACUBONNROCARDTIPBONIMPORTEPORCENTUSRAPROBACIONFCHAPROBACIONFCHENTCODPOSFLGENVUSRSACFECSACHORSACLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02USRCHQFCHCHQHORCHQSEDEDIVDESCODREFIMPDTO2GLOSAIMPDTO2FLGIMPODUSRIMPODFCHIMPODUSRACTFECACTHORACTMOTREPOSICIONVTAPUNTUALCROSSDOCKINGALMACENXDCODORIGENNROORIGENDTALMACENDTEMPAQESPECCLIENTE_RECOGELISTA_DE_PRECIOSCUSTOMERPURCHASEORDERCUSTOMERREQUESTORDERTYPEPERIODCURRENCYPAYFORMDELIVERYDATEREGION1REGION1NAMEREGION2REGION2NAMEREGION3REGION3NAMETELEPHONECONTACTRECEPTORCONTACTRECEPTORNAMEDELIVERYADDRESSCUSTOMERLABELOFFICECUSTOMEROFFICECUSTOMERNAMECUSTOMERSTOCKDEPOCUSTOMERSTOCKDEPONAMECONSOLIDATEINVOICECUSTOMERINVOICECUSTOMERGROUPITEMSPESOVOLUMENEMBALAJE_ESPECIALDELIVERYGROUPDNICLIE-MAILIMPPERCEPCIONPORPERCEPCIONCODPAISCODDEPTCODPROVCODDISTREFERENCEADDRESSIDCUSTOMERFLAGMIGRACIONMIGFECHAMIGHORAMIGUSUARIOTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                       	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /         0 
        1          2 
        3         4         5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          P   "       Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          0  %   �#  t      �#                         Y|a            �#  M                              �  T'                      �+  d'  7_     CODDOCCODCIAFACTORUNDVTACODMATNROPEDCANPEDPREUNIPORDTOIMPDTOIMPLINNROITMAFTIGVAFTISCPREBASPREVTAIMPIGVIMPISCCANATEHORAFLGESTFCHPEDCODAUXMRGUTIPORDTO2TIPVTAPESMATCODCLIALMDESPOR_DSCTOSFLG_FACTORCODDIVCANPICKLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02CANSOLCANAPRIMPDTO2ALMTRFCANTRFLIBRE_D03LIBRE_D04LIBRE_D05LIBRE_F05LIBRE_F03LIBRE_F04SDOCOTCODMATWEBDESMATWEBCANPEDWEBPREUNIWEBIMPLINWEBCUSTOMERARTCODECUSTOMERARTDESCRIPTIONCUSTOMERUNITCODECUSTOMERUNITCODENAMEQTYPRICEROWTOTALCUSTOMERCURRENCYCUSTOMEROLDARTCODEFACTORDESCUENTOTASAIGVIMPORTEUNITARIOSINIMPUESTOIMPORTEREFERENCIALIMPORTEBASEDESCUENTOIMPORTEDESCUENTOIMPORTETOTALSINIMPUESTOMONTOBASEIGVIMPORTEIGVIMPORTETOTALIMPUESTOSIMPORTEUNITARIOCONIMPUESTOCIMPORTEVENTAEXONERADOCIMPORTEVENTAGRATUITOCOTROSTRIBUTOSOPGRATUITOCTIPOAFECTACIONCPREUNISINIMPUESTOCSUMAIMPTETOTALSINIMPUESTOCMONTOBASEIGVCSUMAIGVIMPUESTOBOLSAPLASTICOMONTOTRIBUTOBOLSAPLASTICOCANTIDADBOLSAPLASTICOMONTOUNITARIOBOLSAPLASTICOCIMPORTETOTALCONIMPUESTOIMPORTEBASEDESCUENTONOAFECTOFACTORDESCUENTONOAFECTOIMPORTEDESCUENTONOAFECTO                                                                       	          
                                                                                                                                                                                                                                     !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          �=  &   �#  t      �#                         �#sa            �#  �                              �  �0                      �5  �0  P�     CODMATDESMATCODMARUNDSTKUNDCMPFACEQUCODCTACODNEWMONVTAPREVTAPREBASAFTIGVVINMN1CODCIAVINMN2CODFAMVCTMN1FCHACTCODPR1CODPR2VCTMN2ARTPROFCHUSALFCHUCMPPMAXMN1PMAXMN2PULTMN1PULTMN2USUARIOFCHINGFCHCESFCHALZCLFMATUNDBASSUBFAMCODBRRCODANTTIPARTFCHPRMDFCHPRMHFCHREAPESMATDETALLECANEMPALMACENESDESMARAFTISCPORISCPORVTATPOMRGCTOLISCTOPRMMRGUTIPORMAXFCHMPREUNDANTPREANTPREACTDSCTOSTPOPROPORIGVCTOTOTTPOSUMCTOUNDORDENORDLISORDTMPTPOARTTPOCMBPPCHR__01CHR__02CHR__03DEC__01DEC__02DEC__03DATE__01DATE__02DATE__03MRGUTI-AMRGUTI-BMRGUTI-CPREOFIUNDAUNDBUNDCFLGINTFLGPRECLASEFCHPROMCATCONTATIPROTDSCTOPROMINFORFLGINFORPROMDIVIPROMFCHDPROMFCHHPROMDTODTOVOLRDTOVOLDUNDALTDSCALTMRGALTPREALTLICENCIAPROMMINDIVIPROMMINFCHDPROMMINFCHHPROMMINDTOCODDIGESAVTODIGESALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKMINSTKMAXSTKREPDESCRIPCION-LARGADESCRIPCION-TECNICASW-WEBWEB-SUBCATEGORIALIBRE_D03LIBRE_D04LIBRE_D05PESOBRUTOPAQUETELARGOALTOANCHOCTOLISMARCOCTOTOTMARCOCODSSFAMCLFESPECIALFLGCOMERCIALLIBRE_C06LIBRE_C07LIBRE_C08LIBRE_C09LIBRE_C10CODIGOPADREFACTORPADREREQUIERESERIALNRREQUIEREDUEDATEDTOVOLPTASAIMPUESTOIMPORTEUNITARIOSINIMPUESTODTOVOLPSINIMPUESTOIMPORTEUNITARIOSINIMPUESTO_AIMPORTEUNITARIOSINIMPUESTO_BIMPORTEUNITARIOSINIMPUESTO_CDTOVOLPIMPUESTOIMPORTEUNITARIOIMPUESTOIMPORTEUNITARIOIMPUESTO_AIMPORTEUNITARIOIMPUESTO_BIMPORTEUNITARIOIMPUESTO_C                                                                      	          
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
        p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          � 
        �          �          � 
        �          �          �          � 
        �          �          �          �          4?  '   $  t      $                         �ɺ[            $  im                              �  >                      �>  >  t      CODFAMDESFAMCODCIATPOCMBLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02SWCOMERCIAL                                                                      	          
                                                            �@  (   $  t      $                         �ɺ[            '$  8�                              �  �?                      <@  �?  v      CODCIASUBFAMDESSUBCODFAMORDENSWDIGESALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02                                                                        	          
                                                                          *   k$  t      k$                         �ɺ[            t$  )
                              �  pA                      �A  �A  )      CODCIACODALMUNDVTASTKACTCTOUNIFECHACODMAT                                                                                       ��                                              , ��          �C  D  T �hB            
               11,21,21S,22,34,35                                                                       
             
             
                                         
                                                                                                                T   d   �   �   �   �   �   �   �   �           0  @  P  `  p  �  �      T   d   �   �   �   �   �   �   �   �          0  @  P  `  p  �  �                                                                                                                                     	                  
                                 �F  �F  �F  G   G                        G   G  (G  @G  4G          DG             `G  lG  tG  �G  |G          �G             �G  �G  �G  �G  �G          �G             �G  �G  �G  H   H          H              H  ,H  4H  LH  @H          PH             hH  tH  �H  �H                              �H  �H  �H  �H                              �H  �H  �H  �H                                                                          tt-codmat   X(6)    Codigo Articulo Codigo Articulo     tt-desmat   X(45)   Descripci�n Descripci�n     Descripci�n del material    tt-codfam   X(3)    C�digo  C�digo      C�digo de familia   tt-desfam   X(30)   Descripci�n Descripci�n     Descripci�n de familia  tt-subfam   X(3)    C�digo  C�digo      C�digo de familia   tt-dessub   X(30)   Descripci�n Descripci�n     Descripci�n de familia  tt-qtysoli  ->>,>>9.99  tt-qtysoli  0   tt-qtyate   ->>,>>9.99  tt-qtyate   0   tt-stkcds   ->>,>>9.99  tt-stkcds   0   �  ���
������               �$                �     i     	    �  �  �  �  �  �  �  �      ��                                               j          ����                            �$   ��    �$   ��    �$   ��    �$         �$  $ ��    �$  % �    �$  & ��    �$  ' �'    �$  ( [    �$  *  {    undefined                                                               �       �  �   l   �    ,�                  �����               ��                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     7          assignFocusedWidget         �      �            LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    $       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    6       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          L       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    X       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    d       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    w       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    #      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    7      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    E      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    U      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    f      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    s      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    �    W  �
  �
  P  x       4   ����x       o   X       �
                              �  �   NA  �   �  �   �  �      �      �         $    8    L    `  `  t  
`  �  $  �    �     �      $  i  |  ���                       �     
                    � ߱        �                         � ߱           $  �  �  ���                       �  o   �      4      �                         �       �     �  4  �G  H  �  T     h     |                  (          �  �      ��                  �  �                �                    O   ����    e�          O   ����    R�          O   ����    ��      d  /   �  T                                 3   �����        �  �     �    ��                            ����                                        t                    |                      g                                               �          �  �      ��                  �  �  �              8                    O   ����    e�          O   ����    R�          O   ����    ��            �  �     �    ��                            ����                                                            �                      g                                 $       "�                  �                         � ߱        �  $  �  �  ���                         g              , �                            $          �  �      ��                    	  �              �                    O   ����    e�          O   ����    R�          O   ����    ��      8  @         $          l  @         X              � ߱            $     �  ���                         ��                              ��        j                  ����                                        (                    P                      g                               H  g     $          ,4�                           �          �  �      ��                     �               C                    O   ����    e�          O   ����    R�          O   ����    ��      0              �      4   �����      O     ��  ��  �          L  �      �      4   �����                �                      ��                                      �C                         \  t  /                                    3   ����   D        4                      3   ����            d                      3   ����0          <  }        ��                              ��        j                  ����                                        8                    �                      g                               �  g     `         ,��            ,4�                           <            �      ��                     $              (D                    O   ����    e�          O   ����    R�          O   ����    ��              X  �      H      4   ����H                �                      ��                                      ,�                         h                   |      4   ����|          �     �    ��                              ��        j                  ����                                        �                    (                      g                               8"  g   '  �         ,!4                            �          �  |      ��                 '  )  �              l,�                    O   ����    e�          O   ����    R�          O   ����    ��          (  �  �      �      4   �����      O   (  ��  ��          (  $  �  $        4   ����                �                      ��                  (  (                  -�                       (  4  D     
  
       
       X     
                    � ߱        $  $  (  �  ���                       H  /   (  P     `                          3   ����l  �        �                      3   �����  �        �                      3   �����            �  �                  3   �����      $   (    ���                                                   � ߱        l    (  d  �      �      4   �����                8                      ��                  (  (                  ���                       (  t  �  @         �          (  @                       � ߱        d  $   (  �  ���                           p   (  H  �  T  (  �  �     \  h  t                         � ߱            $  (  �  ���                           (     �  �                         � ߱            $  (  �  ���                           O   (  ��  ��  �        (  �  �  �  �      4   �����     @         �              � ߱            $   (  �  ���                       4  @                    h  @         T          �  @         �          �  @         �             @                       � ߱            $   (  �  ���                                     4                      ��                  (  (                  ��                       (  �        (  P  �      4      4   ����4  �  @         �          �  @         �              � ߱            $   (  `  ���                         ��                              ��        j                  ����                                                            �                      g                               adm-busca       �                                                            �  	                   adm-imprime �   �                                                            �                     _busca-lookup   !  `!  �       h         	     �                          �                       _corre-program  p!  �!              �     
     ,                          (  :                     h�    �  T"  �"      8      4   ����8                �"                      ��                  �  �                  �}�                       �  d"  d#    �  �"  #      l      4   ����l      $  �  8#  ���                       �  @         �              � ߱              �  �#  �#            4   ����      $  �  �#  ���                       T  @         @              � ߱        assignPageProperty                              �$  h$      ��                  E  H  �$              �t�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �$             �$               ��                  �$           ��                            ����                            changePage                              �%  �%      ��                  J  K  �%              �9�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �&  �&      ��                  M  O  �&              �:�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                   '           ��                            ����                            constructObject                             �'  �'      ��                  Q  V  (               ;�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `(             ,(               �� 
  �(             T(  
             ��   �(             |(               �� 
                 �(  
         ��                            ����                            createObjects                               �)  �)      ��                  X  Y  �)              �b�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �*  �*      ��                  [  ]  �*              `c�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �*           ��                            ����                            destroyObject                               �+  �+      ��                  _  `  �+              �ԕ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �,  �,      ��                  b  d  �,              LÕ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �,           ��                            ����                            initializeObject                                �-  �-      ��                  f  g  .              �f�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               /  �.      ��                  i  j  $/              �g�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               0  �/      ��                  l  n  $0              ܌�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  <0           ��                            ����                            notifyPage                              41  1      ��                  p  r  L1              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d1           ��                            ����                            passThrough                             \2  D2      ��                  t  w  t2              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �2             �2               ��                  �2           ��                            ����                            removePageNTarget                               �3  �3      ��                  y  |  �3              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  4             �3  
             ��                  4           ��                            ����                            selectPage                              5  �4      ��                  ~  �  5              XS�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  45           ��                            ����                            toolbar                             (6  6      ��                  �  �  @6              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X6           ��                            ����                            viewObject                              P7  87      ��                  �  �  h7              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                P8  88      ��                  �  �  h8              T��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �8           ��                            ����                            disablePagesInFolder    
      �8       9    I      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder  9      L9      �9    ^      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  `9      �9      �9    r      HANDLE, getCallerWindow �9      �9      :    �      HANDLE, getContainerMode    �9       :      T:    �      CHARACTER,  getContainerTarget  4:      `:      �:    �      CHARACTER,  getContainerTargetEvents    t:      �:      �:    �      CHARACTER,  getCurrentPage  �:      �:      ;    �      INTEGER,    getDisabledAddModeTabs  �:      $;      \;     �      CHARACTER,  getDynamicSDOProcedure  <;      h;      �;  !  �      CHARACTER,  getFilterSource �;      �;      �;  "        HANDLE, getMultiInstanceActivated   �;      �;       <  #        LOGICAL,    getMultiInstanceSupported    <      ,<      h<  $  9      LOGICAL,    getNavigationSource H<      t<      �<  %  S      CHARACTER,  getNavigationSourceEvents   �<      �<      �<  &  g      CHARACTER,  getNavigationTarget �<      �<      0=  '  �      HANDLE, getOutMessageTarget =      8=      l=  (  �      HANDLE, getPageNTarget  L=      t=      �=  )  �      CHARACTER,  getPageSource   �=      �=      �=  *  �      HANDLE, getPrimarySdoTarget �=      �=      >  +  �      HANDLE, getReEnableDataLinks    �=      $>      \>  ,  �      CHARACTER,  getRunDOOptions <>      h>      �>  -  �      CHARACTER,  getRunMultiple  x>      �>      �>  .  �      LOGICAL,    getSavedContainerMode   �>      �>      ?  /        CHARACTER,  getSdoForeignFields �>      $?      X?  0  $      CHARACTER,  getTopOnly  8?      d?      �?  1 
 8      LOGICAL,    getUpdateSource p?      �?      �?  2  C      CHARACTER,  getUpdateTarget �?      �?      @  3  S      CHARACTER,  getWaitForObject    �?      @      H@  4  c      HANDLE, getWindowTitleViewer    (@      P@      �@  5  t      HANDLE, getStatusArea   h@      �@      �@  6  �      LOGICAL,    pageNTargets    �@      �@      �@  7  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �@      4A      dA  8  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  DA      |A      �A  9  �      LOGICAL,INPUT h HANDLE  setCallerWindow �A      �A      �A  :  �      LOGICAL,INPUT h HANDLE  setContainerMode    �A      B      DB  ;  �      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  $B      lB      �B  <  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �B      �B      �B  =  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �B      C      HC  >  
      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  (C      xC      �C  ?  !      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �C      �C       D  @  8      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �C       D      TD  A  H      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   4D      tD      �D  B  [      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �D      �D      E  C  u      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �D      LE      �E  D  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   `E      �E      �E  E  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �E      F      8F  F  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget F      XF      �F  G  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  lF      �F      �F  H  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �F       G      0G  I  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget G      PG      �G  J        LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    dG      �G      �G  K        LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �G      H      @H  L  +      LOGICAL,INPUT phObject HANDLE   setRunDOOptions  H      `H      �H  M  ;      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  pH      �H      �H  N  K      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �H      I      @I  O  Z      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields  I      lI      �I  P  p      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �I      �I      �I  Q 
 �      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �I      J      HJ  R  �      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget (J      lJ      �J  S  �      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    |J      �J      �J  T  �      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �J      K      LK  U  �      LOGICAL,INPUT phViewer HANDLE   getObjectType   ,K      lK      �K  V  �      CHARACTER,  setStatusArea   |K      �K      �K  W  �      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �L  tL      ��                      �L              8�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �M  xM      ��                  
    �M              <��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �N  |N      ��                      �N              �Ó                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �O  �O      ��                      �O              \ē                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �P  �P      ��                      �P              �X�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �P           ��                            ����                            getAllFieldHandles  �K      8Q      lQ  X  �      CHARACTER,  getAllFieldNames    LQ      xQ      �Q  Y  	      CHARACTER,  getCol  �Q      �Q      �Q  Z  	      DECIMAL,    getDefaultLayout    �Q      �Q       R  [  	      CHARACTER,  getDisableOnInit     R      ,R      `R  \  -	      LOGICAL,    getEnabledObjFlds   @R      lR      �R  ]  >	      CHARACTER,  getEnabledObjHdls   �R      �R      �R  ^  P	      CHARACTER,  getHeight   �R      �R      S  _ 	 b	      DECIMAL,    getHideOnInit   �R      $S      TS  `  l	      LOGICAL,    getLayoutOptions    4S      `S      �S  a  z	      CHARACTER,  getLayoutVariable   tS      �S      �S  b  �	      CHARACTER,  getObjectEnabled    �S      �S      T  c  �	      LOGICAL,    getObjectLayout �S       T      PT  d  �	      CHARACTER,  getRow  0T      \T      �T  e  �	      DECIMAL,    getWidth    dT      �T      �T  f  �	      DECIMAL,    getResizeHorizontal �T      �T      �T  g  �	      LOGICAL,    getResizeVertical   �T      U      <U  h  �	      LOGICAL,    setAllFieldHandles  U      HU      |U  i  �	      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    \U      �U      �U  j  
      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �U      �U      $V  k  
      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    V      HV      |V  l  )
      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   \V      �V      �V  m  :
      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �V      �V       W  n  H
      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout  W      DW      tW  o  Y
      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal TW      �W      �W  p  i
      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �W      �W      ,X  q  }
      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated X      TX      �X  r  �
      LOGICAL,    getObjectSecured    hX      �X      �X  s  �
      LOGICAL,    createUiEvents  �X      �X      Y  t  �
      LOGICAL,    bindServer                              �Y  �Y      ��                  �  �  �Y              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �Z  �Z      ��                  �  �  �Z              Ȁ�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �[  �[      ��                  �  �  �[              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �\  �\      ��                       �\              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �]  �]      ��                      �]              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �^  �^      ��                      �^              d��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �_  �_      ��                  	    �_              lF�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �_  
         ��                            ����                            startServerObject                               �`  �`      ��                      a              �$�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                 b  �a      ��                      b              <%�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  0b           ��                            ����                            getAppService   �X      �b      �b  u  �
      CHARACTER,  getASBound  �b      �b       c  v 
 �
      LOGICAL,    getAsDivision   �b      c      <c  w  �
      CHARACTER,  getASHandle c      Hc      tc  x  �
      HANDLE, getASHasStarted Tc      |c      �c  y  �
      LOGICAL,    getASInfo   �c      �c      �c  z 	       CHARACTER,  getASInitializeOnRun    �c      �c      (d  {        LOGICAL,    getASUsePrompt  d      4d      dd  |  %      LOGICAL,    getServerFileName   Dd      pd      �d  }  4      CHARACTER,  getServerOperatingMode  �d      �d      �d  ~  F      CHARACTER,  runServerProcedure  �d      �d      (e    ]      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   e      le      �e  �  p      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   |e      �e      �e  �  ~      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �e      f      Df  �  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   $f      df      �f  � 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    pf      �f      �f  �  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �f      g      <g  �  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   g      \g      �g  �  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  pg      �g      �g  �  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �h  �h      ��                  �  �  �h              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  i             �h  
             ��   4i              i               �� 
                 (i  
         ��                            ����                            addMessage                               j  j      ��                  �  �  8j              |ߕ                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �j             Pj               ��   �j             xj               ��                  �j           ��                            ����                            adjustTabOrder                              �k  �k      ��                  �  �  �k              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
   l             �k  
             �� 
  (l             �k  
             ��                  l           ��                            ����                            applyEntry                              m  �l      ��                  �  �  ,m              ȏ�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  Dm           ��                            ����                            changeCursor                                @n  (n      ��                  �  �  Xn               ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  pn           ��                            ����                            createControls                              lo  To      ��                  �  �  �o              0�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               pp  Xp      ��                  �  �  �p              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                tq  \q      ��                  �  �  �q              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �r  hr      ��                  �  �  �r              �\�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �s  hs      ��                  �  �  �s              `]�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �t  ht      ��                  �  �  �t              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �u  pu      ��                      �u              L�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �v  xv      ��                    	  �v              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �v             �v  
             ��   w             �v               ��   Dw             w               ��                  8w           ��                            ����                            modifyUserLinks                             4x  x      ��                      Lx              h��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �x             dx               ��   �x             �x               �� 
                 �x  
         ��                            ����                            removeAllLinks                              �y  �y      ��                      �y              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �z  �z      ��                      �z              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  {             �z  
             ��   <{             {               �� 
                 0{  
         ��                            ����                            repositionObject                                0|  |      ��                      H|              @��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �|             `|               ��                  �|           ��                            ����                            returnFocus                             �}  h}      ��                    !  �}              쭔                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �}  
         ��                            ����                            showMessageProcedure                                �~  �~      ��                  #  &  �~              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                �~               ��                             ��                            ����                            toggleData                              �  �      ��                  (  *  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  4�           ��                            ����                            viewObject                              ,�  �      ��                  ,  -  D�              `|�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �g      ��      ȁ  � 
 =      LOGICAL,    assignLinkProperty  ��      ԁ      �  �  H      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      `�      ��  �  [      CHARACTER,  getChildDataKey p�      ��      ̂  �  i      CHARACTER,  getContainerHandle  ��      ؂      �  �  y      HANDLE, getContainerHidden  �      �      H�  �  �      LOGICAL,    getContainerSource  (�      T�      ��  �  �      HANDLE, getContainerSourceEvents    h�      ��      ̃  �  �      CHARACTER,  getContainerType    ��      ؃      �  �  �      CHARACTER,  getDataLinksEnabled �      �      L�  �  �      LOGICAL,    getDataSource   ,�      X�      ��  �  �      HANDLE, getDataSourceEvents h�      ��      Ą  �  �      CHARACTER,  getDataSourceNames  ��      Є      �  �        CHARACTER,  getDataTarget   �      �      @�  �  %      CHARACTER,  getDataTargetEvents  �      L�      ��  �  3      CHARACTER,  getDBAware  `�      ��      ��  � 
 G      LOGICAL,    getDesignDataObject ��      ą      ��  �  R      CHARACTER,  getDynamicObject    ؅      �      8�  �  f      LOGICAL,    getInstanceProperties   �      D�      |�  �  w      CHARACTER,  getLogicalObjectName    \�      ��      ��  �  �      CHARACTER,  getLogicalVersion   ��      ̆       �  �  �      CHARACTER,  getObjectHidden ��      �      <�  �  �      LOGICAL,    getObjectInitialized    �      H�      ��  �  �      LOGICAL,    getObjectName   `�      ��      ��  �  �      CHARACTER,  getObjectPage   ��      ȇ      ��  �  �      INTEGER,    getObjectParent ؇      �      4�  �  �      HANDLE, getObjectVersion    �      <�      p�  �        CHARACTER,  getObjectVersionNumber  P�      |�      ��  �        CHARACTER,  getParentDataKey    ��      ��      �  �  -      CHARACTER,  getPassThroughLinks Ԉ       �      4�  �  >      CHARACTER,  getPhysicalObjectName   �      @�      x�  �  R      CHARACTER,  getPhysicalVersion  X�      ��      ��  �  h      CHARACTER,  getPropertyDialog   ��      ĉ      ��  �  {      CHARACTER,  getQueryObject  ؉      �      4�  �  �      LOGICAL,    getRunAttribute �      @�      p�  �  �      CHARACTER,  getSupportedLinks   P�      |�      ��  �  �      CHARACTER,  getTranslatableProperties   ��      ��      ��  �  �      CHARACTER,  getUIBMode  ؊      �      0�  � 
 �      CHARACTER,  getUserProperty �      <�      l�  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    L�      ��      ̋  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ��      �       �  �        CHARACTER,INPUT pcLink CHARACTER    linkProperty     �      D�      t�  �        CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry T�      ��      ܌  �  !      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   ��      H�      x�  �  -      CHARACTER,INPUT piMessage INTEGER   propertyType    X�      ��      ̍  �  ;      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ��      �      $�  �  H      CHARACTER,  setChildDataKey �      0�      `�  �  W      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  @�      ��      ��  �  g      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ��      ܎      �  �  z      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    ��      0�      l�  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled L�      ��      ď  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ��      �      �  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ��      <�      p�  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  P�      ��      ̐  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ��      ��      $�  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �      H�      |�  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  \�      ��      ̑  � 
       LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ��      �       �  �        LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject     �      H�      |�  �  0      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   \�      ��      В  �  A      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      ��      ,�  �  W      LOGICAL,INPUT c CHARACTER   setLogicalVersion   �      H�      |�  �  l      LOGICAL,INPUT cVersion CHARACTER    setObjectName   \�      ��      Г  �  ~      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��      �       �  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion     �      @�      t�  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    T�      ��      Д  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      ��      ,�  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �      L�      ��  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  d�      ��      ؕ  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute ��      ��      ,�  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �      T�      ��  �        LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   h�      ��      �  �        LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  Ȗ      �      8�  � 
 7      LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      X�      ��  �  B      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage h�      ȗ      ��  �  R      LOGICAL,INPUT pcMessage CHARACTER   Signature   ԗ      �      D�  � 	 ^      CHARACTER,INPUT pcName CHARACTER    <�    C	  ��   �      �      4   �����                �                      ��                  D	  q	                  T͓                       D	  ��        E	  ,�  ��      �      4   �����                ��                      ��                  F	  p	                  �͓                       F	  <�  ��    ]	  ԙ  P�      �      4   �����                `�                      ��                  i	  k	                  4Γ                       i	  �         j	                                  D     
                    � ߱        �  $  m	  ��  ���                           $  o	  �  ���                       �                         � ߱        H�    u	  X�  ԛ      �      4   �����                �                      ��                  v	  :
                  �Γ                       v	  h�  �  o   y	   
   ,                                 p�  $   z	  D�  ���                         @                        � ߱        ��  �   {	  4      ��  �   |	  �      ��  �   ~	        ��  �   �	  �      Ԝ  �   �	        �  �   �	  x      ��  �   �	  �      �  �   �	  0      $�  �   �	  �      8�  �   �	        L�  �   �	  �      `�  �   �	        t�  �   �	  �      ��  �   �	  �      ��  �   �	  D      ��  �   �	  �      ĝ  �   �	  �      ؝  �   �	  h      �  �   �	  �       �  �   �	        �  �   �	  �      (�  �   �	        <�  �   �	  �      P�  �   �	  �      d�  �   �	  t      x�  �   �	  �      ��  �   �	  \      ��  �   �	  �      ��  �   �	        Ȟ  �   �	  H      ܞ  �   �	  �      �  �   �	  �      �  �   �	  4      �  �   �	  p      ,�  �   �	  �      @�  �   �	  (      T�  �   �	  d      h�  �   �	  �      |�  �   �	  �      ��  �   �	        ��  �   �	  T      ��  �   �	  �      ̟  �   �	  �      ��  �   �	            �   �	  D                      �          x�  `�      ��                  a
  �
  ��              4�                    O   ����    e�          O   ����    R�          O   ����    ��      �     
  	       	       0                      @!                         � ߱        8�  $ u
  ��  ���                           O   �
  ��  ��  �!               ��          ��  ��    ��                                             ��                            ����                            �!  lK      ��      P�     @     ��                      V ��  �                     �    �
  d�  �      �!      4   �����!                �                      ��                  �
  6                  �r�                       �
  t�  �  �   �
  �!      �  �   �
  `"      ,�  �   �
  �"      @�  �   �
  X#      T�  �   �
  �#      h�  �   �
  P$      |�  �   �
  �$      ��  �   �
  @%      ��  �   �
  �%      ��  �   �
  8&      ̣  �   �
  �&      �  �   �
  ('      ��  �   �
  �'          �   �
   (      �    A  $�  ��      �(      4   �����(                ��                      ��                  B  �                  <�                       B  4�  Ĥ  �   D  �(      ؤ  �   E  d)      �  �   F  �)       �  �   G  T*      �  �   H  �*      (�  �   I  <+      <�  �   J  �+      P�  �   K  ,,      d�  �   L  �,      x�  �   M  -      ��  �   N  �-      ��  �   O  .      ��  �   P  x.      ȥ  �   Q  �.      ܥ  �   R  p/      �  �   S  �/      �  �   T  h0      �  �   U  �0      ,�  �   V  `1      @�  �   W  �1      T�  �   X  X2      h�  �   Y  �2      |�  �   Z  P3      ��  �   [  �3      ��  �   \  H4      ��  �   ]  �4      ̦  �   ^  @5          �   _  �5      ��    �  ��  x�      $6      4   ����$6                ��                      ��                  �  �                  ��                       �  �  ��  �   �  �6      ��  �   �   7      ħ  �   �  |7      ا  �   �  �7      �  �   �  d8       �  �   �  �8      �  �   �  L9      (�  �   �  �9      <�  �   �  �9      P�  �   �  8:      d�  �   �  t:      x�  �   �  �:      ��  �   �  \;      ��  �   �  �;      ��  �   �  L<      Ȩ  �   �  �<      ܨ  �   �  4=      �  �   �  �=      �  �   �  ,>      �  �   �  h>      ,�  �   �  �>      @�  �   �  P?      T�  �   �  �?      h�  �   �   @      |�  �   �  <@      ��  �   �  �@      ��  �   �  �@      ��  �   �  0A      ̩  �      lA      �  �     �A      ��  �     �A      �  �      B      �  �     \B      0�  �     �B      D�  �     C      X�  �     HC      l�  �   	  �C      ��  �   
  �C      ��  �     �C      ��  �     8D      ��  �     tD      Ъ  �     �D      �  �     \E      ��  �     �E      �  �     DF       �  �     �F      4�  �     <G      H�  �     �G      \�  �     4H      p�  �     �H      ��  �     ,I      ��  �     hI      ��  �     �I      ��  �      J      ԫ  �     \J      �  �     �J          �     K      T�  $  �  (�  ���                       tK     
                    � ߱        �    �  p�  ��      �K      4   �����K      /   �  ��     ��                          3   �����K            ܬ                      3   �����K  @�    �  �  ��  p�  �K      4   �����K  	              ��                      ��             	     �  b                  tЕ                       �  �  ��  �   �  ,L       �  $  �  ԭ  ���                       XL     
                    � ߱        �  �   �  xL      l�  $   �  @�  ���                       �L  @         �L              � ߱        (�  $  �  ��  ���                       �L                         � ߱        hM     
  	       	       �M                     4O  @        
 �N              � ߱        ��  V   �  Į  ���                        @O                     tO                     �O                         � ߱        H�  $    T�  ���                       pP     
  	       	       �P                     <R  @        
 �Q              � ߱        ذ  V   !  �  ���                        HR     
  	       	       �R                     T  @        
 �S              � ߱            V   F  t�  ���                        
              8�                      ��             
     d                    �c�                       d  �   T     
  	       	       �T                     �U  @        
 �U          PV  @        
 V          �V  @        
 pV          W  @        
 �V              � ߱            V   y  ��  ���                        adm-clone-props �  d�              �     A     `                          \  �                     start-super-proc    t�  в  �           �     B                                  �                     س      \�  l�      �Z      4   �����Z      /     ��     ��                          3   �����Z            ȳ                      3   �����Z  0�  $  4  �  ���                       �Z                         � ߱        �    D  L�  ȴ  h�  [      4   ����[                <�                      ��                  E  I                  |��                       E  \�  [                     0[                     D[                         � ߱            $  F  ش  ���                             J  ��  ��      \[      4   ����\[  |[                         � ߱            $  K  ��  ���                       �    R  �  �  p�  �[      4   �����[      $  S  D�  ���                       �[                         � ߱            �   p  �[      \     
  	       	       �\                     �]  @        
 �]              � ߱        �  V   �  ��  ���                        (�  �   �  �]      ��    9  D�  T�      ^      4   ����^      /   :  ��     ��                          3   ����,^            ��                      3   ����L^  |�  $  >  �  ���                       h^                         � ߱        �^     
  	       	       _                     ``  @        
  `              � ߱        ��  V   H  �  ���                        ��    �  ĸ  @�      l`      4   ����l`                P�                      ��                  �  �                  ���                       �  Ը      g   �  h�         ,�,�                           0�           �  �      ��                  �      �              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  \�     l�  �`                      3   ����|`  ��     
   ��                      3   �����`         
   ��                      3   �����`    ��                              ��        j                  ����                                        |�              C      ̺                      g                               ��  g   �  ��          ,�	4�                           h�          8�   �      ��                  �  �  P�              Ps                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  �`                      3   �����`            ļ                      3   �����`    ��                              ��        j                  ����                                        ��              D      Լ                      g                               ��  g   �  ��          ,�	<�                           p�          @�  (�      ��                  �  �  X�              �s                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  a                      3   �����`            ̾                      3   ����a    ��                              ��        j                  ����                                        ��              E      ܾ                      g                               ��    �  ��  0�      0a      4   ����0a                @�                      ��                  �                    0�s                       �  Ŀ  ��  /   �  l�     |�                          3   ����@a            ��                      3   ����`a  ��  /  �  ��     ��  �a                      3   ����|a  �     
   �                      3   �����a  H�        8�                      3   �����a  x�        h�                      3   �����a            ��                      3   �����a  ��    �  ��  ��      b      4   ����b      /  �   �     �  �b                      3   ����pb  @�     
   0�                      3   �����b  p�        `�                      3   �����b  ��        ��                      3   �����b            ��                      3   �����b        �  ��  ��      �b      4   �����b      /    (�     8�  Lc                      3   ����,c  h�     
   X�                      3   ����Tc  ��        ��                      3   ����\c  ��        ��                      3   ����pc            ��                      3   �����c  ��    
  �  ��      �c      4   �����c                ��                      ��                                      �dv                         $�      g     ��         ,�\�        �c                  ��          P�  8�      ��                        h�              ,ev                    O   ����    e�          O   ����    R�          O   ����    ��          /    ��     ��  �c                      3   �����c  ��     
   ��                      3   �����c         
   �                      3   �����c    ��                            ����                                        ��              F      �                      g                               P�        d                                     d     
  	       	       �d                     �e  @        
 �e              � ߱        ��  V   �  ��  ���                        �e     
  	       	       pf                     �g  @        
 �g              � ߱        �  V   �  |�  ���                        ��    �  (�  8�      �g      4   �����g      $   �  d�  ���                       4h  @          h              � ߱        d�  g   �  ��         ,��        Hh  ,��        Th                  ��          T�  <�      ��                  �  �  l�              ���                    O   ����    e�          O   ����    R�          O   ����    ��            �  ��  ��      `h      4   ����`h      O  �  ������  th    ��                            ����                                        ��              G      ��                      g                               �  g   �  |�         ,6��         �h                  D�          �  ��      ��                  �  �  ,�              ���                    O   ����    e�          O   ����    R�          O   ����    ��      \�    �  �h  }          O  �  ������  �h    ��                            ����                                        ��              H      t�                      g                               ��  g     (�         ,"��                           ��          ��  ��      ����                   ��              (��                    O   ����    e�          O   ����    R�          O   ����    ��                                                                                                                                               � ߱        ��  $     ��   �                       ��      ��  l�      �h      4   �����h                |�                      ��                    	                  �s                          �  ��  	    ��                                        3   �����h      O    ������  �h  L�    
  ��  p�      �h      4   �����h                ��                      ��                  
                    <t                       
  �  H�  A          ��   ��         ��  \i                                         i   0i                   4�  (�           <i  Li           Di  Ti         �             �   �            d�  ��      �i      4   �����i                ��                      ��                                      \�t                         t�  4�  	    $�                                        3   �����i      O    ������  �i  `�  �     �i      ��  /     ��                                 3   �����i  ��  /     ��                                 3   �����i  �  /     �                                 3   ����j      �     (j        ��                              ��        j                  ����                                              <�              I      (�                      g                               ��  g   !  �         , ��                           ��          ��  ��      ����               "  0  ��              8�u                    O   ����    e�          O   ����    R�          O   ����    ��      $�  $   %  ��  ���                       \j  @         Hj              � ߱        |�  $  &  P�  ���                       hj                         � ߱              (  ��  �      |j      4   ����|j                $�                      ��                  (  /                  Pqv                       (  ��  ��  A  )        ��   ��         t�  �j                                         �j   �j                   ��  ��           �j  �j           �j  �j         �            ��   ��          ,  �  ��      k      4   ����k                ��                      ��                  ,  .                  |��                       ,  �      $   -  ��  ���                       (k  @         k              � ߱                       �                                           ��                              ��        j                  ����                                  �          �  ��         J     (�                      g   $�                                K  �  ��      4k      4   ����4k                ��                      ��                  K  w                  ��s                       K  �  Dk  @                     pk  @         \k          �k  @         �k              � ߱        $�  $   L  ��  ���                        �  g   R  <�         ,n��      }                      �          ��  ��      ��                  S  W  ��              �s                    O   ����    e�          O   ����    R�          O   ����    ��      @�  /  T  0�                                 3   �����k        U  \�  l�      �k      4   �����k      O  V  ������  �k    ��                            ����                                        P�              K      ��                      g                               ��  g   \  8�         ,!��         l                  ,�          ��  ��      ��                  \  ^  ��              ��s                    O   ����    e�          O   ����    R�          O   ����    ��      l  @                         � ߱            $  ]   �  ���                         ��                            ����                                        L�              L      X�                      g                               0�  /   a   �                                 3   ����l        h  L�  ��      8l      4   ����8l                D�                      ��                  h  u                  ��r                       h  \�                ��          l�  T�      ��                 l  s                  �r                       l  ��      O   l    ��          O   l    ��      ��  /   p  ��                                 3   ����Pl        q  ��  ��      pl      4   ����pl      k   r  �              }       n        �   adm-create-objects  ��   �                      M      �                               �                      disable_UI  4�  ��                      N      <                              �   
                   enable_UI   ��  ��                      O                                     �   	                   exitObject  �  `�                      P      �                               �   
                   initializeObject    l�  ��                      Q      �                              !                     procesa-parametros  ��  8�                      R      �                               !                     recoge-parametros   L�  ��                      S      �                               *!                     um-calc-saldos-art  ��  �                    T     L                          H  U!                     um-excel    ,�  ��          �  �  ! " U     \                          X  �#                     um-procesar ��  ��          �      #   V                                 <$                     um-saldos-almacen   ��  X�  �       �      )   W     �                          �  }$                     �   ��   �  ���       �  ! �11,21,21S,22,34,35 � � ���  �       4�  8   ����*   D�  8   ����*   T�  * 
 \�  8   ����(   l�  8   ����(   |�  ( 	 ��  8   ����'   ��  8   ����'   �  '  ��  8   ����&   ��  8   ����&   ��  8   ����%   ��  8   ����%   ��  8   ����$   ��  8   ����$   �  8   ����   �  8   ����   ,�    4�  8   ����   D�  8   ����   T�    \�  8   ����   l�  8   ����   |�        8   ����       8   ����             ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  (�  4�      returnFocus ,INPUT hTarget HANDLE   �  \�  p�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    L�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �  �      removeAllLinks  ,   ��  0�  @�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE  �  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  $�  0�      hideObject  ,   �  D�  \�      editInstanceProperties  ,   4�  p�  ��      displayLinks    ,   `�  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  ��   �      applyEntry  ,INPUT pcField CHARACTER    ��  ,�  <�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER �  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  ��   �      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  T�  d�      unbindServer    ,INPUT pcMode CHARACTER D�  ��  ��      startServerObject   ,   |�  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  ��  �      restartServerObject ,   ��  �  0�      initializeServerObject  ,   �  D�  X�      disconnectObject    ,   4�  l�  ��      destroyServerObject ,   \�  ��  ��      bindServer  ,   ��  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  ��   �      enableObject    ,   ��  �  $�      disableObject   ,   �  8�  D�      applyLayout ,   (�  X�  d�      viewPage    ,INPUT piPageNum INTEGER    H�  ��  ��      viewObject  ,   ��  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  �  0�      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER �  l�  x�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  \�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  ��   �      initPages   ,INPUT pcPageList CHARACTER ��  ,�  H�      initializeVisualContainer   ,   �  \�  h�      hidePage    ,INPUT piPageNum INTEGER    L�  ��  ��      destroyObject   ,   ��  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  ��   �      createObjects   ,   ��  �  $�      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE �  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  ��  ��      changePage  ,   ��  ��  �      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 %     adecomm/as-utils.w 
"   
   �    }        �
"     
   %       	           �     }        �G� �   �G%              � �     %       	  %        %        %       %        %       %               %               %               %              %              %              %               %              
�    � %              %              %              %         %          � A      
�             �G%              %               %     _corre-program  %      ENTRY   
"  
 
   %      ENTRY   
"  
 
   
"   
 �       �    �A� H   
"   
 ��             %               
"   
 �        L     %               (    S    �     }         � U    %               %                   �     }         � b    %     bin/_inslook.r  �     }        �"      � j         �     }         � b    
"   
     �        p     %              � p     
"   
   (    S    �     }         � U    %               %                   �     }         � b    
�     }        �G
�     }        � %     _busca-lookup   �     }        �"      "          "    � A    �
"   
 ��        �     %               
"   
 �             %               
"  
 
 O�        <    6@� w     �      � �   � �     � �   %               
"   
     �        �     � �    
"   
 �        �     %              
"   
 �             �     }         
"   
 ��        H          �     }         �     }        �
"   
 �        �    ��     }        �
"   
 ��        �     %               
"   
   �              %              , (   (     
�     }        �
"   
 �    �     }        �G� �   �G
"   
 ��        �     %               
"   
 �        �     %               %      notify  � �     %      notify  � �     "    �"    �&    &    &    &        %              %              *    "      "      �    �"    �&    &    &    &        %              %              *    "      "      � A    � A      �    }        �� 0     "      � �     %     bin/_calc.r     �  %              
"  
 
   �        �
    B�  � �     %     bin/_calenda.r      �  %              
"  
 
   �        <    B�  � 8     %     recoge-parametros �"      "          "    %              
"  
 
   �        �    B"      %     procesa-parametros �    }        �� A          
"   
 
�    
"   
 
"   
 O    �        T     �        `    
"   
   �        �         �     }        �%              
"   
 
"   
 O    �        �     �        �    
"   
   �        4         �     }        �%              � 
"    
 �%              � �  �         X      $              
�    � h   �     
"   
 �                      
�            � j   O
"    
 O
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"  
 
   �        �    7%               
"  
 
 �           (    1� z  
 � �   �%               o%   o           � �    
"  
 
 �           �    1� �   � �   �%               o%   o           � �   
"  
 
 �               1� �  
 � �   �%               o%   o           � �   
"  
 
 �           �    1� �   � �   �%               o%   o           � �   
"  
 
 �           �    1� �   � �   �%               o%   o           � �   
"  
 
 �           l    1� �   � �   �%               o%   o           %               
"  
 
 ��          �    1�    ��      
"  
 
 �           $    1�    � �   �%               o%   o           � 0  e 
"  
 
 �           �    1� �   � �   �%               o%   o           � �  [ 
"  
 
 �               1�    � �   �%               o%   o           %               
"  
 
 �           �    1�    � �   �%               o%   o           %               
"  
 
 �               1� #   � �   �%               o%   o           %              
"  
 
 ��          �    1� 0   �� �     
"  
 
 �           �    1� ?  
 � �   �%               o%   o           %               
"  
 
 �           8    1� J   � �   �%               o%   o           � �    
"  
 
 ��          �    1� R   ��      
"  
 
 �           �    1� b   � �   �%               o%   o           � x  t 
"  
 
 ��          \    1� �  
 ��      
"  
 
 �           �    1� �   � �   �%               o%   o           � 	  � 
"  
 
 �               1� �   � �   �%               o%   o           � �    
"  
 
 �           �    1� �  
 � �   �%               o%   o           %               
"  
 
 ��           �    1� �   �� �   �%               o%   o           %               
"  
 
 ��           x    1� �   �� �   �%               o%   o           � �    �
"  
 
 ��           �    1� �   �� �   �%               o%   o           o%   o           
"  
 
 ��           h    1� �  
 �� �   �%               o%   o           � �    �
"  
 
 ��           �    1� �   ��   	 �%               o%   o           �   / �
"  
 
 ��          P    1� ;   ��   	   
"  
 
 ��           �    1� M   ��   	 �o%   o           o%   o           � �    �
"  
 
 ��               1� `   ��   	   
"  
 
 ��           <    1� o   ��   	 �o%   o           o%   o           � �    �
"  
 
 ��          �    1�    �� �     
"  
 
 ��          �    1� �   ��   	   
"  
 
 ��          (    1� �   ��   	   
"  
 
 ��          d    1� �   ��   	   
"  
 
 ��           �    1� �   �� �   �o%   o           o%   o           %              
"  
 
 ��              1� �   ��   	   
"  
 
 ��          X    1� �  
 �� �     
"  
 
 ��          �    1� �   ��   	   
"  
 
 ��          �    1� �   ��   	   
"  
 
 ��              1� 	   ��   	   
"  
 
 ��          H    1�    ��   	   
"  
 
 ��          �    1� -  	 ��   	   
"  
 
 ��          �    1� 7   ��   	   
"  
 
 ��          �    1� J   ��   	   
"  
 
 ��           8    1� a   �� �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"  	 
   
"  	 
 �
"  	 
   
"  	 
 O(�  L ( l       �              �� m   � P   �             �@    
� @  , 
�            �� v     p�               �L
�    %              � 8      $     � $         � }          
�    � �     
"  	 
 �� @  , 
�       4!    �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"  
 
 ��           �!    1� �  
 �� �   �%               o%   o           � �    �
"  
 
 ��           T"    1� �  
 �� �   �%               o%   o           o%   o           
"  
 
 ��           �"    1� �   ��    �%               o%   o           o%   o           
"  
 
 ��           L#    1� �   �� �   �%               o%   o           %               
"  
 
 ��           �#    1� �   �� �   �%               o%   o           %               
"  
 
 �           D$    1� �   � �   �%               o%   o           � �    �
"  
 
 ��           �$    1� �   �� �   �%               o%   o           %              
"  
 
 ��           4%    1� �   �� �   �%               o%   o           o%   o           
"  
 
 ��           �%    1� �   �� �   �%               o%   o           o%   o           
"  
 
 ��           ,&    1�   	 �� �   �%               o%   o           � �    �
"  
 
 ��           �&    1�    �� �   �%               o%   o           o%   o           
"  
 
 ��           '    1� &   �� �   �%               o%   o           o%   o           
"  
 
 ��           �'    1� 5   �� �   �%               o%   o           %               
"  
 
 ��           (    1� E   �� �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"  
 
 ��           �(    1� Q   ��   	 �%               o%   o           � �    �
"  
 
 ��           X)    1� ^   ��   	 �%               o%   o           � �    �
"  
 
 ��           �)    1� l   �� �   �%               o%   o           %               
"  
 
 �           H*    1� z   �   	 �%               o%   o           � �    �
"  
 
 ��           �*    1� �   ��   	 �%               o%   o           � �    
"  
 
 ��           0+    1� �   �� �   �%               o%   o           %               
"  
 
 ��           �+    1� �   ��   	 �%               o%   o           � �    �
"  
 
 ��            ,    1� �   ��   	 �%               o%   o           � �    �
"  
 
 ��           �,    1� �   ��   	 �%               o%   o           � �    �
"  
 
 ��           -    1� �   ��   	 �%               o%   o           o%   o           
"  
 
 ��           �-    1� �   ��   	 �%               o%   o           � �    �
"  
 
 �           �-    1� �   �   	 �%               o%   o           � �    �
"  
 
 ��           l.    1� �  	 �� �   �%               o%   o           %               
"  
 
 ��           �.    1�    �� �   �%               o%   o           %               
"  
 
 ��           d/    1�    �� �   �%               o%   o           o%   o           
"  
 
 ��           �/    1� !   �� �   �%               o%   o           o%   o           
"  
 
 ��           \0    1� 0   �� �   �%               o%   o           %               
"  
 
 ��           �0    1� >   �� �   �%               o%   o           %               
"  
 
 ��           T1    1� O   �� �   �%               o%   o           %               
"  
 
 �           �1    1� d   � p   �%               o%   o           %       
       
"  
 
 �           L2    1� x   � p   �%               o%   o           o%   o           
"  
 
 ��           �2    1� �   �� p   �%               o%   o           %              
"  
 
 ��           D3    1� �   �� p   �%               o%   o           o%   o           
"  
 
 ��           �3    1� �   �� p   �%               o%   o           %              
"  
 
 ��           <4    1� �   �� p   �%               o%   o           o%   o           
"  
 
 ��           �4    1� �   �� p   �%               o%   o           %              
"  
 
 ��           45    1� �   �� p   �%               o%   o           o%   o           
"  
 
 �           �5    1� �   �   	 �%               o%   o           � �    �P �L 
�H T   %              �     }        �GG %              
"  
 
 ��           x6    1� �   �� �   �%               o%   o           %               
"  
 
 ��           �6    1� �   �� �   �%               o%   o           o%   o           
"  
 
 ��           p7    1� �   �� �   �%               o%   o           � �    �
"  
 
 ��           �7    1�     �� �   �%               o%   o           �   - �
"  
 
 ��           X8    1� D   �� �   �%               o%   o           � �    �
"  
 
 ��           �8    1� [   �� �   �%               o%   o           � x   �
"  
 
 ��          @9    1� �   ��      
"  
 
 ��           |9    1� �   �� �   �%               o%   o           � �    �
"  
 
 ��          �9    1� �  
 ��      
"  
 
 ��          ,:    1� �   ��      
"  
 
 ��           h:    1� �   ��   	 �%               o%   o           � �    �
"  
 
 ��           �:    1� �   �� �   �%               o%   o           � �    �
"  
 
 ��           P;    1� �   ��    �%               o%   o           o%   o           
"  
 
 ��           �;    1� �   �� �   �%               o%   o           �   ! �
"  
 
 ��           @<    1� '   �� �   �%               o%   o           � �    �
"  
 
 �           �<    1� 4   � �   �%               o%   o           � G   �
"  
 
 �           (=    1� V  	 � �   �%               o%   o           o%   o           
"  
 
 ��           �=    1� `   �� �   �%               o%   o           %               
"  
 
 ��           >    1� l   ��      
"  
 
 ��           \>    1� z   �� �   �%               o%   o           � �   �
"  
 
 ��           �>    1� �   ��   	 �%               o%   o           � �    �
"  
 
 ��           D?    1� �   ��   	 �%               o%   o           � �    �
"  
 
 ��          �?    1� �   ��      
"  
 
 ��          �?    1� �   ��   	   
"  
 
 �           0@    1� �   � �   �o%   o           o%   o           %               
"  
 
 ��          �@    1� �   �� �     
"  
 
 ��          �@    1�    ��   	   
"  
 
 ��          $A    1�    ��   	   
"  
 
 ��          `A    1� .   ��   	   
"  
 
 ��          �A    1� ?   ��   	   
"  
 
 ��          �A    1� P   ��   	   
"  
 
 ��          B    1� a   ��      
"  
 
 ��           PB    1� r   �� �   �%               o%   o           � �  4 �
"  
 
 ��          �B    1� �   ��      
"  
 
 ��           C    1� �   ��      
"  
 
 ��          <C    1� �   ��      
"  
 
 ��          xC    1� �   ��   	   
"  
 
 ��          �C    1� �   ��   	   
"  
 
 ��          �C    1�    ��   	   
"  
 
 ��          ,D    1�     �� �     
"  
 
 ��           hD    1� -   ��   	 �%               o%   o           � �    �
"  
 
 ��           �D    1� ;   ��   	 �%               o%   o           � �    �
"  
 
 ��           PE    1� G   ��   	 �%               o%   o           � �    �
"  
 
 ��           �E    1� \   ��   	 �%               o%   o           � �    �
"  
 
 ��           8F    1� q   �� �   �%               o%   o           %               
"  
 
 ��           �F    1�    �� �   �%               o%   o           o%   o           
"  
 
 ��           0G    1� �   �� �   �%               o%   o           %               
"  
 
 ��           �G    1� �   �� �   �%               o%   o           %               
"  
 
 ��           (H    1� �   �� �   �%               o%   o           o%   o           
"  
 
 ��           �H    1� �   �� �   �%               o%   o           %               
"  
 
 ��           I    1� �   ��   	   
"  
 
 ��           \I    1� �   �� �   �%               o%   o           %              
"  
 
 ��          �I    1� �   ��   	   
"  
 
 ��          J    1�    ��   	   
"  
 
 ��          PJ    1�   
 ��   	   
"  
 
 ��           �J    1�    ��   	 �%               o%   o           � q   �
"  
 
 ��            K    1� -   ��   	 �%               o%   o           � �    �
"   
    "    �%     start-super-proc ��%     adm2/smart.p ,OP �L 
�H T   %              �     }        �GG %              
"  
 
   �        L    6� m     
"  
 
   
�        LL    8
"   
   �        lL    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout O
�H T   %              �     }        �GG %              
"  	 
 O
"  	 
 �
"  	 
 O
"  	 
   (�  L ( l       �        �M    �� m   � P   �        �M    �@    
� @  , 
�       �M    �� v   Op�               �L
�    %              � 8      �M    � $         � }          
�    � �   O
"  	 
 �p� @  , 
�       �N    ��    �p�               �L"    , �   � j   �� l   ��     }        �A      |    "      � j   �%              (<   \ (    |    �     }        �A� n   �A"    �    "    O"    �  < "    O"    �(    |    �     }        �A� n   �A"    �
�H T   %              �     }        �GG %              
"  	 
 O
"  	 
 �
"  	 
 O
"  	 
   (�  L ( l       �        �P    �� m   � P   �        �P    �@    
� @  , 
�       �P    �� v   Op�               �L
�    %              � 8      �P    � $         � }          
�    � �   O
"  	 
 �p� @  , 
�       �Q    �� z  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"  	 
 O
"  	 
 �
"  	 
 O
"  	 
   (�  L ( l       �        �R    �� m   � P   �        �R    �@    
� @  , 
�       �R    �� v   Op�               �L
�    %              � 8      �R    � $         � }          
�    � �   O
"  	 
 �p� @  , 
�       �S    ��    �p�               �L
"   
 , 
�H T   %              �     }        �GG %              
"  	 
   
"  	 
 t
"  	 
   
"  	 
   (�  L ( l       �        lT    �� m   � P   �        xT    �@    
� @  , 
�       �T    �� v     p�               �L
�    %              � 8      �T    � $         � }          
�    � �     
"  	 
 �p� @  , 
�       �U    �� �  
 �p�               �L%     SmartWindow 
"  	 
   p� @  , 
�       V    �� �     p�               �L%      WINDOW  
"  	 
  p� @  , 
�       dV    �� o    p�               �L%               
"  	 
  p� @  , 
�       �V    �� M    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 O    �        �W    �� m   �
"   
   � 8      �W    � $         � }          
�    � �   O
"   
   �        HX    �
"   
   �       hX    /
"   
   
"   
   �       �X    6� m     
"   
   
�        �X    8
"   
   �        �X    �
"   
   �        Y    �
"   
   p�    � �   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 O    �        �Y    �A"    �A
"   
   
�        Z    �@ � 
"   
 �"      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p 2��    �      
�    �     }        �%               %      Server  - �     }        �    "    �� �    �%                   "    �� �    �%      NONE    p�,  8         $     "    �        � 2   O
�    
�H T   %              �     }        �GG %              
"  	 
 O
"  	 
 �
"  	 
 O
"  	 
   (�  L ( l       �        P\    �� m   � P   �        \\    �@    
� @  , 
�       h\    �� v   Op�               �L
�    %              � 8      t\    � $         � }          
�    � �   O
"  	 
 �p� @  , 
�       �]    ��    �p�               �L"    , p�,  8         $     "    �        � @   O
�     "    �%     start-super-proc ��%     adm2/visual.p O�   � h     � d     � f  /   
�H T   %              �     }        �GG %              
"  	 
 O
"  	 
 �
"  	 
 O
"  	 
   (�  L ( l       �        �^    �� m   � P   �        �^    �@    
� @  , 
�       �^    �� v   Op�               �L
�    %              � 8      _    � $         � }          
�    � �   O
"  	 
 �p� @  , 
�       `    �� �   �p�               �L"    , � 
" 
   
 �%     contextHelp 
" 
   
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP ,O%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    � �   �
�    � �   �A    �    � �     
�    � �   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents s%     buildDataRequest ent0 A    �    � �   �
�    �     �%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
" 
   
 �
"   
 �%     contextHelp 
" 
   
   
�    
�    %               
�H T   %              �     }        �GG %              
"  	 
 O
"  	 
 �
"  	 
 O
"  	 
 �(�  L ( l       �        `d    �� m   � P   �        ld    �@    
� @  , 
�       xd    �� v   Op�               �L
�    %              � 8      �d    � $         � }   O     
�    � �   �
"  	 
 �p� @  , 
�       �e    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"  	 
 O
"  	 
 �
"  	 
 O
"  	 
 O(�  L ( l       �        @f    �� m   � P   �        Lf    �@    
� @  , 
�       Xf    �� v   Op�               �L
�    %              � 8      df    � $         � }   O     
�    � �   O
"  	 
 �p� @  , 
�       tg    �� q   �p�               �L%              (        �     }        �G� �   �G� 
"   
 O
"   
   �        h    �%              
"   
 �
"   
 ��     }        �%               
"   
 �%      CLOSE   %                   "    �"    �� M      %                   "    �� A    �%               "      &    &    &    &        %              %               *    � �      %               �    }        �� 0     %     um-procesar %     um-calc-saldos-art %     um-excel ,O�    }        �� A      �            B� A      �            B    "    s� A    �%               "      &    &    &    &        %              %              *    �            B"      � 
"   
 �
"   
 �
"   
 O�        Pk    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � �   	   %               
"   
 �
�    %     createObjects    �     }        �%     initializeObject � �     }        �(        �     }        �G� �   �G� 
"   
 O
"   
   �     }        �
�    
"   
 O"    �"    O"      "      "      "      
"   
 �
"   
   %      CLOSE   %               %      SUPER   �            B         +  %              � �   
 ��            B    +  � �   
   �             B� A      "     �"     �&    &    &    &        %              %              *    �             B           "       � !     "      
"  
 
 O�        �n    �� A      
"  
 
 O�        o    �� A      %                   %              %                   "       %                  "       �     "      �     "      T   "       "      � S!     %               %     um-saldos-almacen �"     "    +  "          "       "       "       � �!     " !     �p                              � �!     p�  �p  �p         %               " !     Pq                              \q                             � �!  	   � �     �  q  q        " !     �q                              �q        �q  �q  �q          � �!     �q         %              � �!     �  |q  �q        " !     �r        �r  �r  �r          �r                              �r                              �r         �  "     � "     � 	"     � "     p�  r   r         %              " !     Hs        0s  4s  8s          Ts                              <s         �  "     � "     � "     p�  �r  �r              � "     "      " !     t         t  t  t          $t                              0t                              t         � %"     � "     � 	"     � "     p�  �s  �s         %              " !     �t        �t  �t  �t          �t                              �t         � %"     � "     � "     p�  dt  pt          H      4          � ("  + ߱    "    O� �   
 �� T"   O    "      � �   
 O" !     �u        �u  �u  �u          �u                               v                              �u         � Z"     � "     � 	"     � "     p�  du  pu         %              " !     �v        �v  �v  �v          �v                              �v         � Z"     � "     � "     p�  4v  @v                    � ]"  (   "      � �"   �" !     |w        dw  hw  lw          �w                              �w                              pw         � �"     � "     � 	"     � "     p�  �v  w         %              " !     ,x        x  x  x          8x                               x         � �"     � "     � "     p�  �w  �w           h   � �"     (   4     "    �� A      � �"  	 O           "      � !   O"    t" !     Xy        @y  Dy  Hy          dy                              py                              Ly         � �"     � "     � 	"     � "     p�  �x  �x         %              " !     z        �y  �y  �y          z                              �y         � �"     � "     � "     p�  �y  �y         � �"     " !     �z        �z  �z  �z          �z                              �z         � �"     � "     � "     p�  @z  Lz         � �"     " !     @{        ({  ,{  0{          L{                              4{         � �"     � "     � "     p�  �z  �z         � �"     " !     �{        �{  �{  �{          �{                              �{         � �"     � "     � "     p�  x{  �{         � �"     " !     x|        `|  d|  h|          �|                              l|         � �"     � "     � "     p�  |   |         � �"  
   " !     }        �|   }  }           }                              }         � �"     � "     � "     p�  �|  �|         � �"     " !     �}        �}  �}  �}          �}                              �}         � �"     � "     � "     p�  L}  X}         �  #     " !     L~        4~  8~  <~          X~                              @~         � #     � "     � "     p�  �}  �}         � #     " !     �~        �~  �~  �~          �~                              �~         � ##     � "     � "     p�  �~  �~         � &#     " !     �        l  p  t          �                              x         � 6#     � "     � "     p�     ,         � 9#  
   %                   " !     %                   " !          � U#     " ! 	    " !     ��        x�  |�  ��          ��                              ��         " ! 
    � "     � "     p�  ,�  8�              � W#     "           � Y#     " ! 	    " !     `�        H�  L�  P�          l�                              T�         " ! 
    � "     � "     p�  ��  �              � W#     "           � [#     " ! 	    " !     0�        �  �   �          <�                              $�         " ! 
    � "     � "     p�  ́  ؁              � W#     "           � ]#     " ! 	    " !      �        �  �  ��          �                              �         " ! 
    � "     � "     p�  ��  ��              � W#     "           � _#     " ! 	    " !     Ѓ        ��  ��  ��          ܃                              ă         " ! 
    � "     � "     p�  l�  x�              � W#     "           � a#     " ! 	    " !     ��        ��  ��  ��          ��                              ��         " ! 
    � "     � "     p�  <�  H�              � W#     "           � c#     " ! 	    " !     p�        X�  \�  `�          |�                              d�         " ! 
    � "     � "     p�  �  �         "           � e#     " ! 	    " !     ,�        �  �  �          8�                               �         " ! 
    � "     � "     p�  ȅ  ԅ         "  	         � g#     " ! 	    " !     �        І  Ԇ  ؆          �                              ܆         " ! 
    � "     � "     p�  ��  ��             "      "  	         � i#     " ! 	    " !     ��        ��  ��  ��          ć                              ��         " ! 
    � "     � "     p�  T�  `�         "  
    " !     �                              � k#     p�  ��  ��         %               " !     |�                              � �!     p�  P�  \�         %              " !     " !     " !     " !     � y#     � �#   �"     �"     �"    �"    �&    &    &    &    &    &    p    L    0        %              %              %                  " $     &        " $     &              " $     � U#         " $     � �#     " $     " $     " $     " $     &    &    &    &    &    &    T    8        %                  " % !   &    %              %              " %     " %     &    &    &    &        %              %              %                  "    �� A    �%                         " &     "          " &     "      %                  " #   �%              " %   �&    &     *    "     �" &   �&    &    &    &        %              %              "     �" &   �" & $  �&    &    &    &    &    &    0        %              %              %              " %     " &     " &     " & $    (   * '   " '     � A      (   * (   " (     � A           "      " %          "  	    " %     %               "     �" )   �" )   �" )   �&    &    &    &    &    &    &    L    0        %              %              %              %              * *   " *                     �           �   l       ��                  4  6  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��          /   5  �      �                           3   �����                                  3   ����	    ��                            ����                                            �           �   l       ��                  @  B  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��          /   A  �      �                           3   ����	                                  3   ����,	    ��                            ����                                            ,          �   l       ���               L  g  �               P��                    O   ����    e�          O   ����    R�          O   ����    ��      �       �              �          (                    �                                         �  A  Q        �   ��         |  p	                                        8	   D	                   �  �           P	  `	           X	  h	         �            �   �          T    �  |  �	      4   �����	                �                      ��                  T  X                  �[�                       T     �	                     �	                         � ߱            $  U  �  ���                                     �                      ��                  Y  e                  XR�                       Y    T  A  Z        �   ��         �  �	                                        �	   �	                   @  4           �	  �	           �	  �	         �                          ]  p  �  <  (
      4   ����(
  0
                     <
                         � ߱            $  ^  �  ���                       H
                     T
                         � ߱            $  b  �  ���                                     �                                           ��                            ����                                                  �           �   l       ��                 q  �  �               �S�                    O   ����    e�          O   ����    R�          O   ����    ��      x  $  s  �   ���                       `
                         � ߱                      �          �  �      ��                 t  �  �              8D�                x     t        O   t    ��          O   t    ��          O   t    ��          p   w  �
  �     �  8  h     �
                x                      ��                  x  |                  �D�                       x  �  �  /   y  �                                 3   �����
        z  �  �      �
      4   �����
      $   {    ���                       �
  @         �
              � ߱        �  �     �
                �                      ��                  }  �                  0E�                       }  H     /   ~  �                                 3   ����             ,            4   ����      $   �  X  ���                       \  @         H              � ߱                   `                                      ��                  �  �                  �|�                       �  �  L  /   �  <                                 3   ����l  �  /   �  x     �                          3   �����            �                      3   �����  <    �  �  �      �      4   �����      $   �    ���                       �  @         �              � ߱            /   �  h                                 3   �����      $  �  �  ���                                                � ߱                     $                                                             ��                            ����                                            �           �   l       ��                 q  �  �               �e�                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       XW     
                    � ߱              �  (  �      �W      4   �����W                �                      ��                  �  �                  ��u                       �  8  �  �  �  �W            �  �  `      TX      4   ����TX                p                      ��                  �  �                  �u                       �  �  �  o   �      ,                                 �  �   �  tX      �  �   �  �X      $  $  �  �  ���                       �X     
                    � ߱        8  �   �  �X      L  �   �  Y      `  �   �  ,Y          $   �  �  ���                       \Y  @         HY              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               0�u                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       �Y     
                    � ߱                  �  �                      ��                   �  �                  ��u                     �  4      4   �����Y      $  �  �  ���                       Z     
                    � ߱        �    �  4  D      0Z      4   ����0Z      /  �  p                               3   ����DZ  �  �   �  PZ          O   �  ��  ��  �Z                               , �                          
                               �      ��                            ����                                                        �   l       ��                  �  �  �               ��r                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  �  �  �               ��s                    O   ����    e�          O   ����    R�          O   ����    ��           �  �   �       �l      4   �����l      n   �     �          �l        �    ,      �l      4   �����l      �   �  �l    ��                            ����                                            $          �   l       ��                  �  �  �               ��s                    O   ����    e�          O   ����    R�          O   ����    ��      �l  �           m  �          m  �           m  �          ,m  �          8m  �              � ߱        �  Z   �  �    �        �l                  �              �              �              �              �              � ߱        �  h   �  P   �        Dm                  
   �  �� �             Pm    ��                              ��        j                  ����                                            �           �   l       ��                  �  �  �               ��s                    O   ����    e�          O   ����    R�          O   ����    ��      �     �  \m  }          O   �  ��  ��  pm    ��                            ����                                            �           �   l       ��                �  �  �               �s                    O   ����    e�          O   ����    R�          O   ����    ��      �   /   �  �                                 3   �����m  H  $   �    ���                       �m  @         �m              � ߱        �  $   �  t  ���                       �m  @         �m              � ߱        �  $   �  �  ���                        n  @         n              � ߱        �  A  �        \   ��         H  dn                                         ,n   8n                   �  �           Dn  Tn           Ln  \n         �            x   �          �  �  X      �n      4   �����n                h                      ��                  �  �                  �Pt                       �  �      $   �  �  ���                       �n  @         �n              � ߱          ��                              ��        j                  ����                                                  �           �   l       ��                  �  �  �               �Qt                    O   ����    e�          O   ����    R�          O   ����    ��          p   �  �n  �       �             o    ��                            ����                                            �           �   l       ��                  �    �               Ss                    O   ����    e�          O   ����    R�          O   ����    ��          p   	  o  �                    0o    ��                            ����                                                       �   l       ��                   4  �               �Ss                    O   ����    e�          O   ����    R�          O   ����    ��            0      �          �  |      ��                  &  1  �              ��r                       &  �       \  �       ��                            7   ����         ��                     �            �                  6   &           ��                    �            �                                                                h  \                                   @            <   L        O   ����  e�          O   ����  R�          O   ����  ��      �  $  '  �  ���                       <o                          � ߱          �      �  X                      ��        0         (  /                  �ft       �o     �     (        $  (  �  ���                       Po                          � ߱        H  $  (    ���                       �o                          � ߱            4   �����o  �  $  )  �  ���                       �o                          � ߱          $  *  �  ���                       p                          � ߱        \  /   ,  4     D                          3   ����$p  t        d                      3   ����Dp  �        �                      3   ����Pp  �        �                      3   ����\p            �                    3   ����`p      $   ,  0  ���                                                    � ߱            $  .  �  ���                       lp                          � ߱        �p       
       
           � ߱            $  0  �  ���                                      @                                                ��                             ��                            ����                                            �           �   l       ��                 :  �  �               lgt                    O   ����    e�          O   ����    R�          O   ����    ��      �   o   P  !         �p                          @  $   S    ���                       �p  @        	 �p              � ߱        �  $  V  l  ���                       hq      !                   � ߱        �  $  Y  �  ���                        r      !                   � ߱        H  $   ]    ���                       �r  @        	 �r              � ߱        �  $   ^  t  ���                       ts  @        	 `s              � ߱        �  $   `  �  ���                       Pt  @        	 <t              � ߱        P  $   a  $  ���                       �t  @        	 �t              � ߱        �  $   d  |  ���                        v  @        	 v              � ߱           $   e  �  ���                       �v  @        	 �v              � ߱        X  $   g  ,  ���                       �w  @        	 �w              � ߱        �  $   h  �  ���                       Xx  @        	 Dx              � ߱          $   k  �  ���                       �y  @        	 |y              � ߱        `  $   l  4  ���                       4z  @        	  z              � ߱        �  $   m  �  ���                       �z  @        	 �z              � ߱          $   n  �  ���                       l{  @        	 X{              � ߱        h  $   o  <  ���                       |  @        	 �{              � ߱        �  $   p  �  ���                       �|  @        	 �|              � ߱          $   q  �  ���                       @}  @        	 ,}              � ߱        p  $   r  D  ���                       �}  @        	 �}              � ߱        �  $   s  �  ���                       x~  @        	 d~              � ߱           $   t  �  ���                         @        	                � ߱        x  $   u  L  ���                       �  @        	 �              � ߱        <	  $  }  �  ���                       �      !                   � ߱              L	      �
          �
  �
      ��                    �  �
              ��r                p       �      x	  �	       ��                            7   ����         ��                     �            
                  6          <
   ��                    �            
                                                                �
  x
                                   @            X
   h
        O   ����  e�          O   ����  R�          O   ����  ��      8  $  �    ���                       �      !                   � ߱        �  $  �  d  ���                       �      ! 	       	           � ߱        �  $  �  �  ���                       �      ! 
       
           � ߱        @  $   �    ���                       ��  @        	 ��              � ߱        �  $  �  l  ���                       ܀      ! 
       
           � ߱        �  $   �  �  ���                       ��  @        	 x�              � ߱        H  $  �    ���                       ��      ! 
       
           � ߱        �  $   �  t  ���                       \�  @        	 H�              � ߱        �  $  �  �  ���                       |�      ! 
       
           � ߱        P  $   �  $  ���                       ,�  @        	 �              � ߱        �  $  �  |  ���                       L�      ! 
       
           � ߱           $   �  �  ���                       ��  @        	 �              � ߱        X  $  �  ,  ���                       �      ! 
       
           � ߱        �  $   �  �  ���                       ̄  @        	 ��              � ߱          $  �  �  ���                       �      ! 
       
           � ߱        `  $   �  4  ���                       ��  @        	 ��              � ߱        �  $  �  �  ���                       ��      ! 
       
           � ߱          $   �  �  ���                       X�  @        	 D�              � ߱        h  $  �  <  ���                       d�      ! 
       
           � ߱        �  $   �  �  ���                       �  @        	  �              � ߱          $  �  �  ���                       4�      ! 
       
           � ߱            $   �  D  ���                       �  @        	 Ї              � ߱        �  $   �  �  ���                       <�  @        	 (�              � ߱           $   �  �  ���                       ��  @        	 ��              � ߱        0  �  �  ��  @  �  �  ��  P  �  �  Ȉ  `  �  �  Ԉ      	  �  �                                        3   ������              !                                               "  T          <  H   ,            ����             J                                        J ! "   ��                             ��                            ����                                            �           �   l       ���               �  �  �               H�t                    O   ����    e�          O   ����    R�          O   ����    ��      0  �   �           @                �  �      ��                  �  �  �              ��r                       �  �       l  �       ��                            7   ����    $      ��               X�    �                              6   �       $ T   ��         0  X�    �                                                                    �   ��   �   �   �                 �  �           (�  8�  H�           0�  @�  P�                      p   �        O   ����  e�          O   ����  R�          O   ����  ��      �    �  $  4      �      4   �����      O   �  �� ��            �      �  �      �  �      ��                  �  �  �              �^s                       �  L  D  �  D       ��                            7   ����    %      ��          	     ��    �            �                  6   �       % �   ��        	 �  ��    �            �                                                        4�   @�   L�   X�                   0  $           d�  t�  ��           l�  |�  ��                      �           p  �       ��$                           A   ����    &      ��          
     <�    �                              6   �       & H   ��        
 4  <�    �                                      *                              �   �                   �  �           �  ,�           $�  4�         �            d   x        O   ����  e�          O   ����  R�          O   ����  ��      L  $  �     ���                       l�      #                   � ߱        �    �  h  �      ��      4   ������                �                      ��                  �  �                  �t                       �  x  L  $  �     ���                       ��      #                   � ߱              �  h  x      ��      4   ������      $  �  �  ���                       ��      #                   � ߱              �  �  h	      �      4   �����                x	                      ��                  �  �                  ��t                       �  �  0
  A  �       �	   ��         �	                                             8�                 
  
           D�           L�         �            �	    
    �    �  L
  �
      T�      4   ����T�                �
                      ��                  �  �                  �Ns                       �  \
  �  A  �       ' <   ��         (  ��                                         `�   l�                   �  �           x�  ��           ��  ��         �            X   l    t  A  �       (   	 ��         �  �                                         Ȍ   Ԍ   ��                 `  T      	     �  ��  �      	     �  �  �         �            $   <    <  9   �     h�                     t�                     ��                     ��                     ��                     ��                         � ߱            $  �  �  ���                       �                     �       	       	           � ߱            $  �  h  ���                                   #                                        #     ��                             ��                             ��                            ����                            �  ( 	 �  '      =   �                           T          �   l       ��               �  �  �               ut                    O   ����    e�          O   ����    R�          O   ����    ��      H$   )    �              �          R$   )                 �          [$   )    8                      c$   )                   ,         �  $  �  �  ���                       (�      )                   � ߱        �  B  �       *   
 ��         �  ��                                         <�   H�   T�   `�                   |  p      
     l�  |�  ��          
     t�  ��  ��  ��                      8   T          �  �  (      �      4   �����                8                      ��                  �  �                  Tt                       �  �      $  �  d  ���                       �      )                   � ߱                    )  �                                     �    )     ��                            ����                                * 
    _   d d     (   ��D"�E"  � �                                               j                                                                         d     D                                                                 P   �� d                                                           �$  G   
 X  �� �d          d                                             J     p      P   �^�d                                                           �$  G   
 X  �^xd                                                        A     w  
    P   �^Xd                                                           �$  G   
 X  �^xd                                                        V     w  
    P   ��d                                                           �$  G   
 X  �xd                                                        6     p     
 X  t\d                                             
           _     p      P   ��	d                                                           �$  G   
 X  ��8d                                                             p      \  pR�p                                                  �$                @       D                                                                    TXS appSrvUtils ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST s-coddiv s-codcia tt-resumen-cot tt-codmat tt-desmat tt-codfam tt-desfam tt-subfam tt-dessub tt-qtysoli tt-qtyate tt-stkcds wWin btnProcesar txtCDs 11,21,21S,22,34,35 txtCodProv txtDesde txtDivision txtHasta txtNomProv fMain X(256) 99/99/9999 GUI Resumenes de Cotizaciones input-var-1 input-var-2 input-var-3 output-var-1 output-var-2 output-var-3 HANDLE-CAMPO BUTTON-LOOKUP PARIENTE load-imagen program_name program_call titulo-look  ENTRY img/b-lookup FRAME,WINDOW FILL-IN ENTRY CHOOSE INTEGER DECIMAL corre_calculadora DATE corre_calendario BROWSE KEYPRESS qbusca ADM-BUSCA qimprime ADM-IMPRIME campo_name PF-G005 Descripci�n de Campos * _BUSCA-LOOKUP OK-SET-WAIT-STATE GENERAL ? _CORRE-PROGRAM DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   txtDesde txtHasta txtCodProv txtCDs btnProcesar CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE Rango de Fechas Erradas gn-prov Maestro de Proveedores Proveedor no existe lProv iStartPage ADM-ERROR ADM-CREATE-OBJECTS DISABLE_UI ENABLE_UI EXITOBJECT 99/99/9999 GN-DIVI DIVISIONES   INITIALIZEOBJECT PROCESA-PARAMETROS RECOGE-PARAMETROS iint cAlm lStkCds lStk , UM-CALC-SALDOS-ART chExcelApplication chWorkbook chWorksheet chWorksheetRange iCount iIndex iColumn cColumn cRange x-signo Excel.Application Visible Workbooks Sheets Item B1 Range Font Bold Value DIVISION :  B2 RESUMEN DE ARTICULOS DE COTIZACIONES - DEL    AL  B3 ALMACENES DE DISTRIBUCION CONSIDERADOS ( ) B4 PROVEEDOR :  < TODOS > A5:R5 A5 COD. ART B5 DESCRIPCION C5 COD.FAM D5 DESCRP. FAM E5 COD.SUBFAM F5 DESCRP. SUBFAM G5 CANT. SOLICITADA H5 CANT. ATENDIDA I5 CANT. X ATENDER J5 STOCKS CDs x-Column x-Range A ' B C D E F G H I J DisplayAlerts Proceso Terminado UM-EXCEL lConsiderar FacCPedi Pedidos al Credito COT W FacDPedi Detalle pedido credito Almmmatg Cat�logo de Materiales Almtfami Tabla de  Familias AlmSFami Tabla de Subfamilias UM-PROCESAR p-almacen p-codmat p-fecha p-Stock AlmStkal AlmStkal UM-SALDOS-ALMACEN idx01 Division Desde Hasta Algun proveedor Centros de Distribucion Procesar IDX01 llave03 llave01 Matg01 fami01 sfam01 <  @,  l  3      & �    H                                         �  �  �     �                                         �  �  T   �                                           	  �   �                                                     �   <                                                  |                                        (  )  L  �                    �                  adm-busca   5  6  �                      �                  adm-imprime A  B  0        $        campo_name  X        H        program_call              p        program_name    �  �     	             �                  _busca-lookup   Q  T  U  X  Y  Z  ]  ^  b  e  g                  OK-SET-WAIT-STATE   �  T     
   �          D                  _corre-program  s  t  w  x  y  z  {  |  }  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��      �        pcProp      ��      �        pcProp      ��              plCancel    @  ��      4        pcProcName  d  ��      X       
 pcProcName  �  ��      |        pcProcName      ��      �       
 pcProcName      ��      �        piPageNum       ��      �        piPageNum       ��              pcPageList      ��      0        pcProc  \  ��      P        pcLinkName      ��      t        pcLinkName  �  ��      �       
 phTarget        ��      �        phTarget        ��      �        piPageNum       ��              pcValue     ��      $        piPageNum       ��      H        pcAction        ��      l       
 phAppService        ��      �        pcMode  �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    (  ��               pcText  H  ��      @        pcText      ��      `        pcText  �  ��      �       
 phObject    �  ��      �       
 phObject        ��      �        phObject        ��      �        pcField     ��              pcCursor    <  ��      0       
 phCaller    `  ��      T        phCaller    �  ��      x        phCaller        ��      �        phCaller    �  ��      �        pcMod   �  ��      �        pcMod       ��       	       
 pcMod   ,	  ��       	       
 phSource    P	  ��      D	        phSource        ��      h	       
 phSource    �	  ��      �	        pdRow       ��      �	        pdRow       ��      �	       
 hTarget �	  ��      �	        pcMessage       ��      
        pcMessage       ��      4
        plEnabled             T
     cType     �
     @   @
          �
                  getObjectType   u
  �
  �
  �
        �
  
   hReposBuffer    �
        �
  
   hPropTable             
   hBuffer             
   hTable  \
  d     A   �
          T                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �            �  
   hProc             �        pcProcName  $  ,  	   B   �  �                        start-super-proc    �  �  �  �  �  �  �  �  �  �  �     C                                   �  P  �     D                                   �  �  �  �     E                                   �  �  �  $     F                                     �  X     G                                   �  �  (  �     H                                   �  �  �  `  �     I                                           	  
                                    (     lProv   �  `  	   J                                 %  &  (  )  ,  -  .  /  0  0  �     K                                   T  U  V  W  �  �     L                                   ]  ^  �  @     M               ,                  adm-create-objects  �  �  �     N               t                  disable_UI  �  �  �  �  D  �     O               �                  enable_UI   �  �  �  �  �       P                                 exitObject  �  �  �  �  h  	   Q               T                  initializeObject    �  �  �  �  �  �  �  �  �  $  �     R               �                  procesa-parametros  �  �  �  �  �  $     S                                 recoge-parametros   	  
      P         H     iint    l         d     cAlm    �         �     lStkCds            �     lStk    �  �     T   4          �                  um-calc-saldos-art  &  '  (  )  *  ,  .  /  0  1  4  <  !      (     chExcelApplication  \  !      P     chWorkbook  |  !      p     chWorksheet �  !      �     chWorksheetRange    �  !      �     iCount  �  !      �     iIndex  �  !      �     iColumn   !   	        cColumn 0  !   
   (     cRange  L  !      D     x-signo l  "     `     x-Column        "     �     x-Range �  �  8   U             �                  um-excel    P  S  V  Y  ]  ^  `  a  d  e  g  h  k  l  m  n  o  p  q  r  s  t  u  }    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �      #      �     lConsiderar �        V   �          �                  um-procesar �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  |  )      p        p-almacen   �  )      �        p-codmat    �  )      �        p-fecha     )      �        p-Stock �  $     W       X                        um-saldos-almacen   �  �  �  �  �  �  �  �      " d      �                          |  �  	   tt-resumen-cot  �                                    (         4         @         L         X         tt-codmat   tt-desmat   tt-codfam   tt-desfam   tt-subfam   tt-dessub   tt-qtysoli  tt-qtyate   tt-stkcds   �          x  
   appSrvUtils �        �     s-coddiv    �        �     s-codcia    �       �  
   wWin    �       �     txtCDs              txtCodProv  <       0     txtDesde    \       P     txtDivision |       p     txtHasta    �       �     txtNomProv  �       �     input-var-1 �       �     input-var-2 �       �     input-var-3              output-var-1    D       4     output-var-2    h    	   X     output-var-3    �    
   |  
   HANDLE-CAMPO    �       �  
   BUTTON-LOOKUP   �       �  
   PARIENTE    �       �     load-imagen             program_name    8       (     program_call    X       L     titulo-look �  	      l  
   gshAstraAppserver   �  
      �  
   gshSessionManager   �   	     �  
   gshRIManager    �   
     �  
   gshSecurityManager            
   gshProfileManager   H        0  
   gshRepositoryManager    t        \  
   gshTranslationManager   �        �  
   gshWebManager   �        �     gscSessionId    �        �     gsdSessionObj           �  
   gshFinManager   (          
   gshGenManager   L        <  
   gshAgnManager   p        `     gsdTempUniqueID �        �     gsdUserObj  �        �     gsdRenderTypeObj    �        �     gsdSessionScopeObj  �    	   �  
   ghProp      
     
   ghADMProps  @       0  
   ghADMPropsBuf   h       T     glADMLoadFromRepos  �       |     glADMOk �       �  
   ghContainer �       �     cObjectName �       �     iStart          �     cAppService              cASDivision L       4     cServerOperatingMode    h       `     cFields          |     iStartPage  �    L  �  tt-resumen-cot  �       �  PF-G005 �       �  gn-prov �       �  GN-DIVI    $       FacCPedi    (   %      FacDPedi    D   &    8  Almmmatg    `   '    T  Almtfami    |   (    p  AlmSFami         *    �  AlmStkal             7   W  X  i  �  �  �        '  �  �  �  �  �  �  �  C	  D	  E	  F	  ]	  i	  j	  k	  m	  o	  p	  q	  u	  v	  y	  z	  {	  |	  ~	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  :
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
  6  A  B  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                   	  
                                        �  �  �  �  �  �  �  �  �  �  �  �    !  F  b  d  y        4  D  E  F  I  J  K  R  S  p  �  �  9  :  >  H  �  �  �  �  �  �  �  �  �  �  �  �  �      
          �  �  �  �  �  �    !  K  L  R  \  a  h  l  p  q  r  s  u  w      H� % C:\Progress\OpenEdge\src\adm2\windowmn.i �#  f!  C:\Progress\OpenEdge\src\adm2\containr.i �#  � $ %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    0$  ��  C:\Progress\OpenEdge\src\adm2\visual.i   t$  # # %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �$  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    �$  �� " %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i    %  I�  C:\Progress\OpenEdge\src\adm2\smart.i    d%  Ds ! C:\Progress\OpenEdge\gui\fn  �%  tw   %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �%  Q.  C:\Progress\OpenEdge\gui\set  &  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i (&  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    \&  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �&  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �&  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i '  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i X'  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �'  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �'  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    (  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i T(  �j  C:\Progress\OpenEdge\gui\get �(  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �(  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �(  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i 8)  Su  C:\Progress\OpenEdge\src\adm2\globals.i  l)  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �)  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �)  �  C:\Progress\OpenEdge\src\adm2\appsprto.i $*  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   X*  �X 
 C:\Progress\OpenEdge\src\adm2\visprto.i  �*  !� 	 %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �*  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i +  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    L+  ��  %d:\newsie\on_in_co\src\adm-vm\method\vmviewer.i  �+  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �+  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   ,  �   d:\newsie\on_in_co\APLIC\vta2\w-resumen-cotizaciones.w       1  z      �,     ?  %   �,  �   �      �,  �   �     �,     �     �,  �   �     �,     x     �,  �   p     �,       $   -  �        -     �  !   $-  �   �     4-     �  !   D-  �   �     T-     �  !   d-  r   �     t-  n   �     �-     g  #   �-  i   b     �-     @     �-  P   '     �-  �        �-     �  "   �-  �   �     �-     �     .  �   �     .     |     $.  �   z     4.     X     D.  g   >     T.          d.  O        t.  �   �     �.     �  !   �.  �   _     �.           �.  �   �     �.     �     �.  �   �     �.     �     �.  �   �     /     �     /  �   �     $/     q     4/  �   `     D/     >     T/  �   ;     d/          t/  }        �/     �     �/     o     �/     !     �/     �     �/  7   �     �/  �   �     �/  O   �     �/     o     0     !     0  �   �     $0  �   �     40  O   �     D0     �     T0     c     d0  �   >     t0  x   6     �0  M   !     �0          �0     �
     �0  a   �
     �0  �  �
     �0     m
     �0  �  :
     �0  O   ,
     1     
     1     �	     $1  �   �     41     �     D1          T1  x        d1     �     t1     �     �1     �     �1     p     �1     W     �1  Q   G     �1     �     �1     �     �1     �     �1     �     2  f   \     2     �  
   $2  "   �     42     �  	   D2     �     T2  Z   1     d2     9     t2     �     �2     �     �2     �     �2     �     �2  �   �      �2     q     �2  '   �       �2     @      �2            3           