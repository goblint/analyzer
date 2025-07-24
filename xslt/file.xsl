<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="sht">
	  <span>
		  <xsl:attribute name="class">sh <xsl:value-of select="@type" /></xsl:attribute>
		  <xsl:value-of select="." />
	  </span>
  </xsl:template>


  <xsl:template match="file">
    <xsl:for-each select="ln" > 
      <div class="sl">

        <xsl:choose>
          <xsl:when test="@wrn='[]' and @ns='[]'">
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="onclick">select_line(<xsl:value-of select="@nr"/>,<xsl:value-of select="@ns"/>,<xsl:value-of select="@wrn"/>)</xsl:attribute>
          </xsl:otherwise>
        </xsl:choose>
        
        <xsl:attribute name="id">ln<xsl:value-of select="@nr" /></xsl:attribute>
        <span>
          <xsl:choose>
            <xsl:when test="@ded='true'">
              <xsl:attribute name="class">sl-nr dead</xsl:attribute>
            </xsl:when>
            <xsl:when test="@ns='[]'">
              <xsl:attribute name="class">sl-nr</xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="class">sl-nr live</xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:value-of select="@nr" />
        </span>
        <span>
          <xsl:choose>
            <xsl:when test="@wrn='[]'">
              <xsl:attribute name="class">sl-nowarn</xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="class">sl-warn</xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
        </span>
        <span class="sl-data">
          <xsl:apply-templates />
        </span>
      </div>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="/">
    <html>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <link rel="stylesheet" href="../style.css" type="text/css"/>
        <script type="text/javascript" src="../jquery-2.1.0.min.js"/>
        <script type="text/javascript" src="../jquery.iframeResizer.min.js"></script>
        <!--<script type="text/javascript" src="../iframeResizer.contentWindow.min.js"/>-->
        <script type="text/javascript" src="../script.js"/>
      </head>
      <body onload="init_file()">
        <div class="source-block">
          <xsl:apply-templates select="file" />
        </div>
      </body>
    </html>
  </xsl:template>

</xsl:stylesheet>
