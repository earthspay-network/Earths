package com.earthspay.lang

import com.earthspay.lang.v1.BaseGlobal

package object hacks {
  private[lang] val Global: BaseGlobal = com.earthspay.lang.Global // Hack for IDEA
}
