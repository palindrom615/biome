/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import Generator from '../../Generator';
import {RegExpStartCharacter} from '@romejs/js-ast';

export default function RegExpStartCharacter(generator: Generator) {
  generator.append('^');
}
