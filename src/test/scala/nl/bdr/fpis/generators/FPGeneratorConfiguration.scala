package nl.bdr.fpis.generators

import org.scalatest.prop.Configuration

trait FPGeneratorConfiguration extends Configuration {
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200, sizeRange = 30)
}
