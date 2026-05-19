<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


  <xsl:template match="map">
    <xsl:choose>
      <xsl:when test="normalize-space(.) != '' or ./@* != ''">
        <xsl:for-each select="key" >
          <xsl:choose>
            <xsl:when test="following-sibling::*[1]//value">
              <div class="toggle">
                <span>
                  <xsl:value-of select="." /> &#8594;
                </span>
                <div>
                  <xsl:apply-templates select="following-sibling::*[1]" />
                </div>
              </div>
            </xsl:when>
            <xsl:otherwise>
              <div class="nontoggle">
                <span class="emph">
                  <xsl:value-of select="." /> &#8594;
                </span>
                <span class="emph">
                  <xsl:apply-templates select="following-sibling::*[1]" />
                </span>
              </div>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>

      </xsl:when>
      <xsl:otherwise>
        &#8709;
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="set">
    <xsl:choose>
      <xsl:when test="normalize-space(.) != '' or ./@* != ''">
          <xsl:for-each select="value" >
            <div class="nontoggle">
              <xsl:apply-templates select="." />
            </div>
          </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        &#8709;
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="analysis">
    <xsl:choose>
      <xsl:when test="value//value">
        <div class="toggle">
          <span>
            <xsl:value-of select="@name" />
          </span> &#8594;
          <div>
            <xsl:apply-templates select="value" />
          </div>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <div class="nontoggle">
          <span class="emph">
            <xsl:value-of select="@name" />
          </span> &#8594;
          <span class="emph">
            <xsl:apply-templates select="value" />
          </span>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="path">
    <xsl:choose>
      <xsl:when test="analysis//value">
        <div class="toggle">
          <span>path:</span>
          <div>
            <xsl:apply-templates select="analysis" />
          </div>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <div class="nontoggle">
          <span>path:</span>
          <span>
            <xsl:apply-templates select="analysis" />
          </span>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="call">
    <a target="_top" class="node-wrap">
        <xsl:attribute name="href">../frame.html?file=<xsl:value-of select="@file"/>&amp;fun=<xsl:value-of select="@fun"/>&amp;node=<xsl:value-of select="@id" /></xsl:attribute>
        <div class="node-id">
        Node:<xsl:value-of select="@id" />
      </div>
      <div class="node-location">
        @<xsl:value-of select="@file" />:<xsl:value-of select="@line" />:<xsl:value-of select="@column" />-<xsl:value-of select="@endLine" />:<xsl:value-of select="@endColumn" /> (synthetic: <xsl:value-of select="@synthetic" />)
      </div>
    </a>
    <div class="toggle off">
      <span>context:</span>
      <div>
        <xsl:apply-templates select="context" />
      </div>
    </div>
    <xsl:apply-templates select="path" />
  </xsl:template>


  <xsl:template match="/">
    <html>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <link rel="stylesheet" href="../style.css" type="text/css"/>
        <script type="text/javascript" src="../jquery-2.1.0.min.js"/>
        <script type="text/javascript" src="../iframeResizer.contentWindow.min.js"/>
        <script type="text/javascript" src="../script.js"/>
      </head>
      <body onload="init_node()">
        <xsl:apply-templates select="loc/call" />
      </body>
    </html>
  </xsl:template>

</xsl:stylesheet>