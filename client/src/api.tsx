import * as R from 'ramda'
import * as Bindings from './generated/bindings'

export const makeApi = bearerToken => R.map(
  f => R.partialRight(f, [bearerToken]),
  Bindings.default
)

