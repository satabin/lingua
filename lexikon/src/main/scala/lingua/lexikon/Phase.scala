/* Copyright (c) 2016 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package lingua
package lexikon

abstract class Phase[O <: Options, T](val name: Option[String]) {

  protected[this] def process(options: O, reporter: Reporter): T

  final def run(options: O, reporter: Reporter): T = {

    for (name <- name) {
      if (options.verbose) {
        reporter.info(f"beginning of phase $name")
      }
    }

    val start = System.currentTimeMillis

    val t = process(options, reporter)

    val end = System.currentTimeMillis

    for (name <- name) {
      if (options.verbose) {
        reporter.info(f"end of phase $name")
      }
      if (options.timing) {
        val time = end - start
        reporter.info(f"phase $name ran in $time ms")
      }
    }

    if (reporter.hasErrors) {
      reporter.summary()
      sys.exit(1)
    }

    t

  }

  def flatMap[U](f: T => Phase[O, U]): Phase[O, U] =
    new FlatMappedPhase(this, f)

  def map[U](f: T => U): Phase[O, U] =
    new MappedPhase(this, f)

  def filter(p: T => Boolean): Phase[O, T] =
    new FilteredPhase(this, p)

  def withFilter(p: T => Boolean): Phase[O, T] =
    new FilteredPhase(this, p)

}

private class FilteredPhase[O <: Options, T](phase: Phase[O, T], p: T => Boolean) extends Phase[O, T](None) {

  def process(options: O, reporter: Reporter): T = {
    val t = phase.run(options, reporter)

    if (!p(t)) {
      reporter.error("Returned value does not satisfy predicate")
    }

    t
  }

}

private class FlatMappedPhase[O <: Options, T, U](p1: Phase[O, T], f: T => Phase[O, U]) extends Phase[O, U](None) {

  def process(options: O, reporter: Reporter): U = {
    val t = p1.run(options, reporter)
    f(t).run(options, reporter)
  }

}

private class MappedPhase[O <: Options, T, U](p: Phase[O, T], f: T => U) extends Phase[O, U](None) {

  def process(options: O, reporter: Reporter): U =
    f(p.run(options, reporter))

}
