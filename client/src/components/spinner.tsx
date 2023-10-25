import styled, { css } from 'styled-components'

const Spinner = styled.div`
    height: 40px;
    width: 40px;
    border: 5px solid black;
    border-bottom-color: transparent;
    border-radius: 50%;
    box-sizing: border-box;
    animation: rotation 1s linear infinite;

    @keyframes rotation {
        0%   { transform: rotate(0deg);   }
        100% { transform: rotate(360deg); }
    }
`
const Inner = styled.div<{ size: 'small' | 'medium' }>`
    height: 40px;
    width:  40px;
    ${props => props.size === 'small' && css`
        transform: scale(0.5) translate(-50%, -50%);
    `}
`
const Root = styled.div<{ size: 'small' | 'medium' }>`
    height: ${props => props.size === 'small' ? '20px' : '40px'};
    width: ${props => props.size === 'small' ? '20px' : '40px'};
    position: relative;
`
const Expand = styled.div`
    height: 100%;
    width: 100%;
    display: flex;
    align-items: center;
    justify-content: center;
`

export default (props: { size?: 'small' | 'medium', expand?: boolean }) => {
  const { size = 'medium', expand = false } = props;
  const component = (
    <Root size={size}>
      <Inner size={size}>
        <Spinner />
      </Inner>
    </Root>
  )
  return expand ? <Expand>{component}</Expand> : component;
}

