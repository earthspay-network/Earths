package com.earthspay.history

import com.earthspay.db.WithState
import com.earthspay.settings.EarthsSettings
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Assertion, Suite}

trait DomainScenarioDrivenPropertyCheck extends WithState { _: Suite with GeneratorDrivenPropertyChecks =>
  def scenario[S](gen: Gen[S], bs: EarthsSettings = DefaultEarthsSettings)(assertion: (Domain, S) => Assertion): Assertion =
    forAll(gen) { s =>
      withDomain(bs) { domain =>
        assertion(domain, s)
      }
    }
}
