<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template name="filename">
    <xsl:param name="path"/>
    <xsl:choose>
      <xsl:when test="contains($path, '/')">
        <xsl:call-template name="filename">
          <xsl:with-param name="path" select="substring-after($path, '/')"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$path"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="text">
    <li>
      <a target="_parent">
        <xsl:attribute name="href">../frame.html?file=<xsl:call-template name="filename"><xsl:with-param name="path" select="@file"/></xsl:call-template>&amp;line=<xsl:value-of select="@line"/></xsl:attribute>
        <xsl:call-template name="filename"><xsl:with-param name="path" select="@file"/></xsl:call-template> @ <xsl:value-of select="@line"/>
      </a>:
      <xsl:value-of select="." />
    </li>
  </xsl:template>

  <xsl:template match="warning/group">
    <xsl:value-of select="@name" />:
    <ul>
      <xsl:apply-templates select="text" /> 
    </ul>
  </xsl:template>

  <xsl:template match="/">
    <html>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <link rel="stylesheet" href="../style.css" type="text/css"></link>
      </head>
      <body>
        <xsl:apply-templates select="warning" /> 
      </body>
    </html>
  </xsl:template>

</xsl:stylesheet>