<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="report">
    <xsl:for-each select="file">
      <xsl:variable name="file" select="@name" />
      <a>
        <xsl:attribute name="href">frame.html?file=<xsl:value-of select="$file"/></xsl:attribute>
        <xsl:value-of select="@name" /> :
      </a>
      <ul>
        <xsl:for-each select="function">
          <xsl:variable name="fun" select="@name" />
          <li>
            <a>
              <xsl:attribute name="href">frame.html?file=<xsl:value-of select="$file"/>&amp;fun=<xsl:value-of select="$fun"/></xsl:attribute>
              <xsl:value-of select="@name" /> 
            </a>
          </li>
        </xsl:for-each>
      </ul>      
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="/">
    <html>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <!-- <link rel="stylesheet" href="style.css" type="text/css"></link> -->
        <title>Analysis Report</title>
      </head>
      <body>
        <xsl:apply-templates select="report" />
      </body>
    </html>
  </xsl:template>

</xsl:stylesheet>