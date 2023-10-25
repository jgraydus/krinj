import styled from 'styled-components'

const Button = styled.button`
    padding: 5px;
    border-radius: 6px;
    border-style: solid;
    border-width: 1px;
    user-select: none;

    &:enabled {
        border-color: black;
        background-color: #111;
        color: #DDD !important;
        cursor: pointer;
        &:focus { outline: thin dashed white; }
        &:hover { background-color: #333; }
        &:active { background-color: #666; }
    }

    &:disabled {
        border-color: #444;
        background-color: #111;
        color: #444 !important;
        cursor: not-allowed;
        &:hover { background-color: #111; }
    }
`

export default Button;

