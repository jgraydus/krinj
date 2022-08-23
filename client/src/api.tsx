import * as R from 'ramda'
import * as Bindings from './generated/bindings'

const bearerToken = 'eyJhbGciOiJIUzI1NiJ9.eyJfdXNlcklkIjoiYTJhYjIwMzctMjYxNC00MmYx' +
                    'LWI3MjMtZThiN2JlZGQ4NWYyIn0.5j9d5E_9ypS1W4kQNGjWVAfVbR2Fh_2sQA484-BGyiY';

const makeApi = bearerToken => R.map(
  f => R.pipe(
         // the bearer token is always the last argument in the generated code
         R.partialRight(f, [bearerToken]),
         // get the returned data rather than the entire request
         R.andThen(R.prop('data'))
       ),
  Bindings.default
)

const api = makeApi(bearerToken);

export default api;
