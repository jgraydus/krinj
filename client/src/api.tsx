import * as R from 'ramda'
import * as Bindings from './generated/bindings'

export const makeApi = bearerToken => R.map(
  f => R.pipe(
         // the bearer token is always the last argument in the generated code
         R.partialRight(f, [bearerToken]),
         // get the returned data rather than the entire request
         R.andThen(R.prop('data'))
       ),
  Bindings.default
)

