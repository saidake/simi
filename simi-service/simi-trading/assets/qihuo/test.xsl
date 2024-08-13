<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.1" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format" exclude-result-prefixes="fo">

    <xsl:template match="/ ">
        <xsl:param name="data" select="0" />
        <fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">

            <fo:layout-master-set>
                <fo:simple-page-master master-name="A4" page-height="29.7cm" page-width="21cm" margin-top="2cm"
                                       margin-bottom="2cm" margin-left="2cm" margin-right="2cm">
                    <fo:region-body/>
                </fo:simple-page-master>
            </fo:layout-master-set>

            <fo:page-sequence master-reference="A4">
                <fo:flow flow-name="xsl-region-body">
                    <fo:inline-container inline-progression-dimension="15.0%"   >
                        <fo:inline>Test Block1</fo:inline>
                    </fo:inline-container>
                    <fo:block-container block-progression-dimension="auto" >
                        <fo:block >Test Block2</fo:block>
                        <fo:block font-size="10pt">
                            <fo:table table-layout="fixed" width="100%" border-collapse="separate">
                                <fo:table-column column-width="4cm"/>
                                <fo:table-column column-width="4cm"/>
                                <fo:table-body>
                                </fo:table-body>
                            </fo:table>
                        </fo:block>
                    </fo:block-container>
                </fo:flow>
            </fo:page-sequence>
        </fo:root>
    </xsl:template>

</xsl:stylesheet>
