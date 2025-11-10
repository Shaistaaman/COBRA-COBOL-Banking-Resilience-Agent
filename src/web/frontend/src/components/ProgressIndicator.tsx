import './ProgressIndicator.css'

function ProgressIndicator () {
  return (
    <div className='progress-indicator'>
      <div className='spinner'></div>
      <div className='progress-steps'>
        <div className='step active'>
          <div className='step-icon'>🔍</div>
          <div className='step-label'>Parsing COBOL</div>
        </div>
        <div className='step active'>
          <div className='step-icon'>🧠</div>
          <div className='step-label'>Analyzing Logic</div>
        </div>
        <div className='step active'>
          <div className='step-icon'>📝</div>
          <div className='step-label'>Generating Explanation</div>
        </div>
        <div className='step active'>
          <div className='step-icon'>☁️</div>
          <div className='step-label'>Creating AWS Code</div>
        </div>
      </div>
    </div>
  )
}

export default ProgressIndicator
