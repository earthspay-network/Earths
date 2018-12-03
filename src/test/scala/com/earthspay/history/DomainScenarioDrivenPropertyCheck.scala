package com.earthspay.history

import com.earthspay.db.WithState
import com.earthspay.settings.EarthsSettings
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait DomainScenarioDrivenPropertyCheck extends WithState { _: GeneratorDrivenPropertyChecks =>
  def scenario[S](gen: Gen[S], bs: EarthsSettings = DefaultEarthsSettings)(assertion: (Domain, S) => Assertion): Assertion =
    forAll(gen) { s =>
      withDomain(bs) { domain =>
        assertion(domain, s)
      }
    }
}
