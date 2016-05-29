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

abstract class Phase[T](val name: Option[String]) {

  protected[lexikon] def process(options: Options, reporter: Reporter): T

  def run(options: Options, reporter: Reporter): T = {

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

  def flatMap[U](f: T => Phase[U]): Phase[U] =
    new FlatMappedPhase(this, f)

  def map[U](f: T => U): Phase[U] =
    new MappedPhase(this, f)

  def filter(p: T => Boolean): Phase[T] =
    new FilteredPhase(this, p)

  def withFilter(p: T => Boolean): Phase[T] =
    new FilteredPhase(this, p)

}

private class FilteredPhase[T](phase: Phase[T], p: T => Boolean) extends Phase[T](None) {

  def process(options: Options, reporter: Reporter): T = {
    val t = phase.process(options, reporter)

    if (!p(t)) {
      reporter.error("Returned value does not satisfy predicate", -1)
    }

    t
  }

}

private class FlatMappedPhase[T, U](p1: Phase[T], f: T => Phase[U]) extends Phase[U](None) {

  def process(options: Options, reporter: Reporter): U = {
    val t = p1.run(options, reporter)
    f(t).run(options, reporter)
  }

}

private class MappedPhase[T, U](p: Phase[T], f: T => U) extends Phase[U](None) {

  def process(options: Options, reporter: Reporter): U =
    f(p.run(options, reporter))

}
