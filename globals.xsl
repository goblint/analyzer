<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="map">
    <xsl:choose>
      <xsl:when test="normalize-space(.) != '' or ./@* != ''">
        map:

        <xsl:for-each select="key" >
          <xsl:choose>
            <xsl:when test="following-sibling::*[1]//value">
              <div class="toggle">
                <span>
                  <xsl:value-of select="." /> &#8594;
                </span>
                <span>
                  <xsl:apply-templates select="following-sibling::*[1]" />
                </span>
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
        set:
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
                    <span>
                        <xsl:apply-templates select="value" />
                    </span>
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
  <xsl:template match="glob">
    <div class="toggle">
        <span>
            <xsl:value-of select="key" />
        </span> &#8594;
        <span>
            <xsl:apply-templates select="analysis" />
        </span>
    </div>
  </xsl:template>
  
  <xsl:template match="globs">
    <xsl:apply-templates select="glob" />
  </xsl:template>

  <xsl:template match="/">
    <html>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <link rel="stylesheet" href="../style.css" type="text/css"></link>
        <script type="text/javascript" src="../script.js"></script>
        <script type="text/javascript" src="../jquery-2.1.0.min.js"></script>
        <script type="text/javascript" src="../iframeResizer.contentWindow.min.js"/>
        <script type="text/javascript" src="../fileData.js"/>
      </head>
      <body onload="init_all()">
        <xsl:apply-templates select="globs" />
      </body>
    </html>
  </xsl:template>

</xsl:stylesheet>