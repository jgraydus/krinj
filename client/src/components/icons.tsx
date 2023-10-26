import styled from 'styled-components'

export const IconRoot: any = styled.div<{ size?: number, padding?: number, direction?: 'up' | 'down' }>`
    width: ${props => props.size || 40}px;
    height: ${props => props.size || 40}px;
    padding: ${props => props.padding || 2}px;
    display: flex;
    align-items: center;
    justify-content: center;
    transform: rotate(${props => props.direction === 'down' ? 180 : 0}deg);
`

export const CarotIcon =
    ({ 
        size = 14,
        direction = 'up',
        onClick = () => {},
        color = "black"
    } : {
        size?: number,
        direction?: 'up' | 'down',
        onClick?: () => any,
        color?: string,
    }) => (
    <IconRoot size={size} direction={direction} onClick={onClick}>
        <svg viewBox="0.25 0.0 8.1 9.0" xmlns="<http://www.w3.org/2000/svg>">
           <path
              fill={color}
              fillRule="evenodd"
              d="M 4.2332313,0.60068734 0.08112573,7.7930306 H 1.9913886 L 4.28642,3.8185975 6.5808856,7.7930306 h 1.8050173 z"
              strokeWidth="0.289707"
           />
        </svg>
    </IconRoot>
);

